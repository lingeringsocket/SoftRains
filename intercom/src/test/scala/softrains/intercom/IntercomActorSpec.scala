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
package softrains.intercom

import softrains.base._

import akka.actor._
import akka.testkit._

import scala.concurrent.duration._

class IntercomActorSpec extends AkkaActorSpecification
{
  // speaker output is shared, so we need isolation
  sequential

  import IntercomActor._

  "IntercomActor" should
  {
    "encounter protocol errors" in new AkkaActorExample
    {
      val parent = TestProbe()
      val actor = parent.childActorOf(Props(classOf[IntercomActor]))

      actor ! UnpairMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))
      parent.expectMsg(WokeUpMsg)

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR))

      actor ! PartnerListenMsg()
      expectMsg(ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR))

      actor ! PairRequestMsg
      expectMsg(PairAcceptedMsg)

      actor ! PairRequestMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED))

      actor ! RingtoneMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_RING_WHILE_PAIRED))
    }

    "have a very short conversation" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[IntercomActor]))

      actor ! RingtoneMsg
      expectMsg(SpeakerSoundFinishedMsg(None))

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg(None))

      actor ! PairRequestMsg
      expectMsg(PairAcceptedMsg)

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(10 seconds, SpeakerSoundFinishedMsg(None))

      actor ! PartnerListenMsg()
      expectMsg(SilenceMsg)

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg(None))

      actor ! UnpairMsg
    }

    "provide a busy signal" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[IntercomActor]))

      val probe = TestProbe()
      actor.tell(PairRequestMsg, probe.ref)
      probe.expectMsg(PairAcceptedMsg)

      actor ! RingtoneMsg
      expectMsg(BusyMsg)

      actor ! PairRequestMsg
      expectMsg(BusyMsg)

      actor ! DoorbellMsg
      expectMsg(SpeakerSoundFinishedMsg(None))

      actor ! UnpairMsg
      expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))

      actor ! PartnerUtteranceMsg("hello")
      expectMsg(ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR))

      actor ! PartnerListenMsg()
      expectMsg(ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR))

      actor.tell(UnpairMsg, probe.ref)

      actor ! PairRequestMsg
      expectMsg(PairAcceptedMsg)
    }

    "support preemption" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[IntercomActor]))

      val probe = TestProbe()
      actor.tell(PairRequestMsg, probe.ref)
      probe.expectMsg(PairAcceptedMsg)

      actor ! PairRequestMsg
      expectMsg(BusyMsg)

      actor ! PairPreemptMsg
      expectMsg(PairAcceptedMsg)

      probe.expectMsg(PreemptionDisconnectMsg)

      actor.tell(UnpairMsg, probe.ref)
      probe.expectMsg(ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR))

      actor ! PartnerUtteranceMsg("sorry to interrupt")
      expectMsg(10 seconds, SpeakerSoundFinishedMsg(None))
    }
  }
}
