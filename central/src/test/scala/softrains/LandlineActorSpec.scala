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

import akka.actor._
import akka.testkit._

import org.specs2.mutable._

import scala.concurrent.duration._

abstract class AkkaActorSpecification extends Specification
{
  abstract class AkkaActorExample extends TestKit(ActorSystem())
      with After
      with ImplicitSender
  {
    def after =
      TestKit.shutdownActorSystem(system, Duration.Inf, true)
  }
}

class LandlineActorSpec extends AkkaActorSpecification
{
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
    }

    "have a very short conversation" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      actor ! PairRequestMsg(voice)
      expectMsg(PairAcceptedMsg)

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(UtteranceFinishedMsg)

      actor ! PartnerListenMsg
      expectMsg(SilenceMsg)

      actor ! UnpairMsg
    }

    "provide a busy signal" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[LandlineActor]))

      val probe = TestProbe()
      actor.tell(PairRequestMsg(voice2), probe.ref)
      probe.expectMsg(PairAcceptedMsg)

      actor ! PairRequestMsg(voice)
      expectMsg(BusyMsg)

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
      expectMsg(UtteranceFinishedMsg)
    }
  }
}
