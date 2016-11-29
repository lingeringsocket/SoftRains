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

import com.ibm.watson.developer_cloud.text_to_speech.v1._
import com.ibm.watson.developer_cloud.text_to_speech.v1.model._
import com.ibm.watson.developer_cloud.text_to_speech.v1.util._

import sys.process._

object LandlineActor
{
  val PROTOCOL_ALREADY_PAIRED = "already paired"

  val PROTOCOL_UNPAIR_WITHOUT_PAIR = "cannot unpair without pairing first"

  val PROTOCOL_UTTERANCE_WITHOUT_PAIR = "must be paired before utterance"

  val PROTOCOL_LISTEN_WITHOUT_PAIR = "must be paired before listening"

  val VOICE_NONE = "none"

  sealed trait State
  sealed trait Data

  // received messages
  final case class PairRequestMsg(voiceName : String)
  final case class PairPreemptMsg(voiceName : String)
  final case class PartnerUtteranceMsg(utterance : String)
  case object PartnerListenMsg
  case object UnpairMsg

  // sent messages
  case object BusyMsg
  final case class ProtocolErrorMsg(error : String)
  case object PairAcceptedMsg
  case object PreemptionDisconnectMsg
  final case class PersonUtteranceMsg(utterance : String)
  case object UtteranceFinishedMsg
  case object SilenceMsg

  case object Active extends State

  final case class Partner(actorRef : ActorRef, voiceName : String)
      extends Data
}
import LandlineActor._

class LandlineActor extends LoggingFSM[State, Data]
{
  private val settings = CentralActorSettings(context)

  private val unpaired = ActorRef.noSender

  private val tts = new TextToSpeech

  override def preStart()
  {
    if (isWatsonEnabled) {
      tts.setUsernameAndPassword(
        settings.WatsonTts.user, settings.WatsonTts.password)
    }
  }

  startWith(Active, Partner(unpaired, VOICE_NONE))

  when(Active) {
    case Event(PairRequestMsg(voiceName), Partner(oldPartner, _)) => {
      if (sender == oldPartner) {
        oldPartner ! ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED)
        stay
      } else if (oldPartner == unpaired) {
        sender ! PairAcceptedMsg
        stay using Partner(sender, voiceName)
      } else {
        sender ! BusyMsg
        stay
      }
    }
    case Event(PairPreemptMsg(voiceName), Partner(oldPartner, _)) => {
      if (oldPartner != unpaired) {
        oldPartner ! PreemptionDisconnectMsg
      }
      sender ! PairAcceptedMsg
      stay using Partner(sender, voiceName)
    }
    case Event(UnpairMsg, Partner(partner, _)) => {
      if (sender == partner) {
        stay using Partner(unpaired, VOICE_NONE)
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerUtteranceMsg(utterance), Partner(partner, voiceName)) => {
      if (sender == partner) {
        log.info("Say '" + utterance + "' using voice " + voiceName)
        say(utterance, voiceName)
        partner ! UtteranceFinishedMsg
        stay
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerListenMsg, Partner(partner, _)) => {
      if (sender == partner) {
        log.info("Listening...")
        partner ! SilenceMsg
        stay
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR)
        stay
      }
    }
  }

  private def isWatsonEnabled = !settings.WatsonTts.user.isEmpty

  private def say(utterance : String, voiceName : String)
  {
    if (!isWatsonEnabled) {
      return
    }
    val stream = tts.synthesize(
      utterance, Voice.EN_ALLISON, AudioFormat.WAV)
    val in = WaveUtils.reWriteWaveHeader(stream.execute)
    (settings.Speaker.command #< in).!
  }

  initialize()
}
