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

import com.ibm.watson.developer_cloud.text_to_speech.v1._
import com.ibm.watson.developer_cloud.text_to_speech.v1.model._
import com.ibm.watson.developer_cloud.text_to_speech.v1.util._

import com.ibm.watson.developer_cloud.speech_to_text.v1._
import com.ibm.watson.developer_cloud.speech_to_text.v1.model._
import com.ibm.watson.developer_cloud.speech_to_text.v1.websocket._

import com.ibm.watson.developer_cloud.http._

import sys.process._

import javax.sound.sampled._

import scala.concurrent._
import scala.concurrent.duration._

import com.ibm.watson.developer_cloud.text_to_speech.v1.model.{
  AudioFormat => WatsonAudioFormat
}
import javax.sound.sampled.{
  AudioFormat => JavaAudioFormat
}

object IntercomActor
{
  val PROTOCOL_ALREADY_PAIRED = "already paired"

  val PROTOCOL_UNPAIR_WITHOUT_PAIR = "cannot unpair without pairing first"

  val PROTOCOL_UTTERANCE_WITHOUT_PAIR = "must be paired before utterance"

  val PROTOCOL_LISTEN_WITHOUT_PAIR = "must be paired before listening"

  val PROTOCOL_RING_WHILE_PAIRED = "cannot ring phone while paired"

  val VOICE_NONE = "none"

  sealed trait State
  sealed trait Data

  // received messages
  trait SpeakerSoundMsg extends PeripheralMsg
  final case class PairRequestMsg(voiceName : String)
      extends PeripheralMsg
  final case class PairPreemptMsg(voiceName : String)
      extends PeripheralMsg
  final case class PartnerUtteranceMsg(utterance : String)
      extends SpeakerSoundMsg
  case object PartnerListenMsg
      extends PeripheralMsg
  case object UnpairMsg
      extends PeripheralMsg
  case object RingtoneMsg
      extends SpeakerSoundMsg
  case object DoorbellMsg
      extends SpeakerSoundMsg

  // sent messages
  case object BusyMsg
      extends PeripheralMsg
  final case class ProtocolErrorMsg(error : String)
      extends PeripheralMsg
  case object PairAcceptedMsg
      extends PeripheralMsg
  case object PreemptionDisconnectMsg
      extends PeripheralMsg
  final case class PersonUtteranceMsg(utterance : String)
      extends PeripheralMsg
  case object SpeakerSoundFinishedMsg
      extends PeripheralMsg
  case object SilenceMsg
      extends PeripheralMsg

  case object Active extends State

  final case class Partner(actorRef : ActorRef, voiceName : String)
      extends Data
}
import IntercomActor._

class IntercomActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val unpaired = ActorRef.noSender

  private val tts = new TextToSpeech

  private val stt = new SpeechToText

  override def preStart()
  {
    if (isWatsonEnabled) {
      tts.setUsernameAndPassword(
        settings.WatsonTts.user, settings.WatsonTts.password)
      stt.setUsernameAndPassword(
        settings.WatsonStt.user, settings.WatsonStt.password)
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
        partner ! SpeakerSoundFinishedMsg
        stay
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerListenMsg, Partner(partner, _)) => {
      if (sender == partner) {
        log.info("Listening...")
        if (isWatsonEnabled) {
          val cl = classOf[javax.sound.sampled.AudioSystem].getClassLoader
          val old = Thread.currentThread.getContextClassLoader
          try {
            Thread.currentThread.setContextClassLoader(cl)
            listen(partner)
          } finally {
            Thread.currentThread.setContextClassLoader(old)
          }
        } else {
          partner ! SilenceMsg
        }
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR)
      }
      stay
    }
    case Event(RingtoneMsg, Partner(partner, _)) => {
      if (partner == unpaired) {
        log.info("Ring ring")
        val command = settings.Speaker.ringtoneCommand
        if (!command.isEmpty) {
          command.!
        }
        sender ! SpeakerSoundFinishedMsg
      } else if (partner == sender) {
        sender ! ProtocolErrorMsg(PROTOCOL_RING_WHILE_PAIRED)
      } else {
        sender ! BusyMsg
      }
      stay
    }
    case Event(DoorbellMsg, Partner(partner, _)) => {
      log.info("Ding dong")
      val command = settings.Speaker.doorbellCommand
      if (!command.isEmpty) {
        command.!
      }
      sender ! SpeakerSoundFinishedMsg
      stay
    }
  }

  private def isWatsonEnabled =
    !settings.WatsonTts.user.isEmpty && !settings.WatsonStt.user.isEmpty

  private def say(utterance : String, voiceName : String)
  {
    if (!isWatsonEnabled) {
      return
    }
    val stream = tts.synthesize(
      utterance, Voice.EN_ALLISON, WatsonAudioFormat.WAV)
    val in = WaveUtils.reWriteWaveHeader(stream.execute)
    (settings.Speaker.command #< in).!
  }

  private def listen(partner : ActorRef)
  {
    var result : AnyRef = SilenceMsg
    val sampleRate = 44100
    val format = new JavaAudioFormat(sampleRate, 16, 1, true, false)
    val info = new DataLine.Info(classOf[TargetDataLine], format)
    val line = AudioSystem.getLine(info).asInstanceOf[TargetDataLine]
    line.open(format)
    line.start
    try {
      val audio = new AudioInputStream(line)
      val options = (new RecognizeOptions.Builder).
        contentType(HttpMediaType.AUDIO_RAW + "; rate=" + sampleRate).
        maxAlternatives(1).build
      val speechPromise = Promise[SpeechResults]()
      val speechFuture = speechPromise.future
      val disconnectPromise = Promise[Object]()
      val disconnectFuture = disconnectPromise.future
      stt.recognizeUsingWebSocket(
        audio, options, new BaseRecognizeCallback {
          override def onTranscription(speechResults : SpeechResults)
          {
            audio.close
            val transcript =
              speechResults.getResults.get(speechResults.getResultIndex)
            val utterance = transcript.getAlternatives.get(0).getTranscript.trim
            log.info("Heard:  " + utterance)
            result = PersonUtteranceMsg(utterance)
            speechPromise.success(speechResults)
          }

          override def onError(e : Exception)
          {
            audio.close
            // FIXME proper error handling
            speechPromise.failure(e)
          }

          override def onDisconnected()
          {
            disconnectPromise.success(null)
          }
        })
      Await.ready(disconnectFuture, Duration.Inf)
      Await.ready(speechFuture, Duration.Inf)
    } finally {
      line.stop
      line.close
      log.info("Done listening.")
      partner ! result
    }
  }

  initialize()
}
