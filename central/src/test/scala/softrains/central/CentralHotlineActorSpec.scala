// SoftRains:  a Genuine People Personality for your home
// Copyright 2016-2017 John V. Sichi
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
package softrains.central

import softrains.base._
import softrains.conversation._
import softrains.intercom._

import akka.actor._
import akka.testkit._

class CentralHotlineActorSpec
    extends AkkaActorSpecification
{
  import IntercomActor._

  protected val central = new CentralService(settings, new MockCableRouterMonitor)
  
  "CentralHotlineActor" should
  {
    "repeat whatever is said" in new AkkaActorExample
    {
      central.setActorSystem(system)
      val probe = TestProbe()
      val actor = system.actorOf(Props(new CentralHotlineActor(central) {
        override def say(utterance : String, voice : String)
        {
          super.say(utterance, voice)
          probe.ref ! PartnerUtteranceMsg(utterance, voice)
        }
      }))
      val partnerVoice = ConversationPartner.MICHAEL.voiceName
      def partnerUtteranceMsg(utterance : String) =
        PartnerUtteranceMsg(utterance, partnerVoice)
      probe.expectMsg(partnerUtteranceMsg("Hotline active!"))
      actor ! PersonUtteranceMsg("I am Groot.", "Groot")
      probe.expectMsg(partnerUtteranceMsg("i am groot."))
      expectNoMsg
    }
  }
}
