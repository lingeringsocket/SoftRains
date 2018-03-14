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

import softrains.intercom._
import softrains.conversation._

import akka.actor._

object CentralHotlineActor
{
  val INTERCOM_NAME = "hotline"
}

class CentralHotlineActor(
  central : CentralService)
    extends Actor with ActorLogging
{
  import IntercomActor._
  import CentralHotlineActor._

  val intercom = new CentralIntercom(
    INTERCOM_NAME,
    context, central, () => { self })

  override def preStart()
  {
    log.info("Starting hotline")
    central.addIntercom(intercom)
    val topicSource = new SequentialTopicSource(Seq(new EchoTopic))
    val dispatcher = new TopicDispatcher(
      topicSource, "", "Polly wants a cracker!")
    central.activateConversation(
      intercom,
      dispatcher,
      ConversationPartner.MICHAEL)
  }

  override def postStop()
  {
    central.removeIntercom(intercom)
  }

  private def say(voice : String, utterance : String)
  {
    val openhab = new CentralOpenhab(
      central.getActorSystem, central.getSettings)
    openhab.sendCommand("VoiceResponse", utterance)
  }

  def receive =
  {
    case pum : PersonUtteranceMsg => {
      intercom.getConversationActor ! pum
    }
    case PairRequestMsg => {
      sender ! PairAcceptedMsg
    }
    case UnpairMsg => {
      log.info("Hotline connection closed by central")
      context.stop(self)
    }
    case PartnerUtteranceMsg(utterance, voice) => {
      say(voice, utterance)
      sender ! SpeakerSoundFinishedMsg()
    }
    case SystemUtteranceMsg(utterance, voice) => {
      say(voice, utterance)
      sender ! SpeakerSoundFinishedMsg()
    }
      // FIXME:  play sounds over webaudio, including DoorbellMsg
  }
}
