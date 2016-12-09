// SoftRains:  a Genuine People Personality for your home
// Copyright 2016 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package softrains

import com.typesafe.config._

import akka.actor._
import akka.testkit._

import org.specs2.mutable._

import scala.concurrent.duration._

import java.util.concurrent.atomic._

object AkkaActorSpecification
{
  private val suffixGenerator = new AtomicLong
}

abstract class AkkaActorSpecification(confFile : String = "test.conf")
    extends Specification
{
  protected val settings = CentralSettings(loadConfig(confFile))

  protected def configureSystem(overrideConf : String) =
    ActorSystem(
      "TestActors_" + AkkaActorSpecification.suffixGenerator.incrementAndGet,
      loadConfig(overrideConf))

  protected def loadConfig(overrideConf : String) =
  {
    val actualConf = {
      if (overrideConf.isEmpty) {
        confFile
      } else {
        overrideConf
      }
    }
    if (actualConf.isEmpty) {
      ConfigFactory.load
    } else {
      ConfigFactory.load(actualConf)
    }
  }

  abstract class AkkaActorExample(overrideConf : String = "test.conf")
      extends TestKit(configureSystem(overrideConf))
      with After
      with ImplicitSender
  {
    def after =
      TestKit.shutdownActorSystem(system, Duration.Inf, true)
  }
}

class LandlineActorSpec extends AkkaActorSpecification
{
  // speaker output is shared, so we need isolation
  sequential

  import LandlineActor._

  private val voice = "charlie the unicorn"

  private val voice2 = "magical liopleurodon"

  "LandlineActor" should
  {
    "encounter protocol errors" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      actor ! UnpairMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR))

      actor ! PartnerListenMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR))

      actor ! PairRequestMsg(voice)
      expectMsg(PairAcceptedMsg)

      actor ! PairRequestMsg(voice)
      expectMsg(ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED))

      actor ! RingtoneMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_RING_WHILE_PAIRED))
    }

    "have a very short conversation" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      actor ! RingtoneMsg
      expectMsg(SpeakerSoundFinishedMsg)

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg)

      actor ! PairRequestMsg(voice)
      expectMsg(PairAcceptedMsg)

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(10 seconds, SpeakerSoundFinishedMsg)

      actor ! PartnerListenMsg
      expectMsg(SilenceMsg)

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg)

      actor ! UnpairMsg
    }

    "provide a busy signal" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      val probe = TestProbe()
      actor.tell(PairRequestMsg(voice2), probe.ref)
      probe.expectMsg(PairAcceptedMsg)

      actor ! RingtoneMsg
      expectMsg(BusyMsg)

      actor ! PairRequestMsg(voice)
      expectMsg(BusyMsg)

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg)

      actor ! UnpairMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR))

      actor ! PartnerListenMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR))

      actor.tell(UnpairMsg, probe.ref)

      actor ! PairRequestMsg(voice)
      expectMsg(PairAcceptedMsg)
    }

    "support preemption" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      val probe = TestProbe()
      actor.tell(PairRequestMsg(voice2), probe.ref)
      probe.expectMsg(PairAcceptedMsg)

      actor ! PairRequestMsg(voice)
      expectMsg(BusyMsg)

      actor ! PairPreemptMsg(voice)
      expectMsg(PairAcceptedMsg)

      probe.expectMsg(PreemptionDisconnectMsg)

      actor.tell(UnpairMsg, probe.ref)
      probe.expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))

      actor ! PartnerUtteranceMsg("sorry to interrupt")
      expectMsg(10 seconds, SpeakerSoundFinishedMsg)
    }
  }
}
