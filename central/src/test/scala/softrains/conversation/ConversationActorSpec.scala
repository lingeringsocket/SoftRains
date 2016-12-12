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
package softrains.conversation

import softrains.base._
import softrains.central._
import softrains.intercom._

import akka.actor._

class ConversationActorSpec extends AkkaActorSpecification
{
  import ConversationActor._
  import IntercomActor._

  "ConversationActor" should
  {
    "have a one-way conversation" in new AkkaActorExample
    {
      val actor = system.actorOf(Props(classOf[ConversationActor]))
      val typhlosion = new HomeResident("Typhlosion")
      val greeting = new DailyGreeting(typhlosion)
      actor ! ActivateMsg(greeting, self)
      expectMsg(PairRequestMsg(voiceName))
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg("Good morning, Typhlosion!"))
      actor ! SpeakerSoundFinishedMsg
      expectMsg(UnpairMsg)
    }
  }
}

