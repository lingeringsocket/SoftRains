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

import java.io._
import java.net._

import org.apache.commons.io.output._

import scala.concurrent._
import scala.concurrent.duration._

import com.bitsinharmony.recognito._

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
  final case class PairRequestMsg(voice : String)
      extends PeripheralMsg
  final case class PairPreemptMsg(voice : String)
      extends PeripheralMsg
  final case class PartnerUtteranceMsg(utterance : String)
      extends SpeakerSoundMsg
  case class PartnerListenMsg(newPersonName : String = "")
      extends PeripheralMsg
  case object UnpairMsg
      extends PeripheralMsg
  case object RingtoneMsg
      extends SpeakerSoundMsg
  case object DoorbellMsg
      extends SpeakerSoundMsg
  case class StartAudioFileMsg(audioFile : String, loop : Boolean)
      extends SpeakerSoundMsg
  case object StopAudioFileMsg
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
  final case class PersonUtteranceMsg(utterance : String, personName : String)
      extends PeripheralMsg
  case object SpeakerSoundFinishedMsg
      extends PeripheralMsg
  case object SilenceMsg
      extends PeripheralMsg

  case object Active extends State

  final case class Partner(
    actorRef : ActorRef, voice : String,
    background : Option[Process] = None)
      extends Data
}
import IntercomActor._

class IntercomActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val unpaired = ActorRef.noSender

  private val tts = new TextToSpeech

  private val stt = new SpeechToText

  private val audioDir = settings.Files.audioPath

  private var first = true

  private val recognito = new Recognito[String](44100.0f)

  private var personCount = 0

  override def preStart()
  {
    if (isWatsonEnabled) {
      tts.setUsernameAndPassword(
        settings.WatsonTts.user, settings.WatsonTts.password)
      stt.setUsernameAndPassword(
        settings.WatsonStt.user, settings.WatsonStt.password)
    }
    if (!audioDir.isDirectory) {
      if (!audioDir.mkdirs) {
        throw new IOException(
          "Unable to create audio directory " + audioDir)
      }
    }
  }

  startWith(Active, Partner(unpaired, VOICE_NONE))

  when(Active) {
    case Event(PairRequestMsg(voice), Partner(oldPartner, _, bg)) => {
      if (sender == oldPartner) {
        oldPartner ! ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED)
        stay
      } else if (oldPartner == unpaired) {
        sender ! PairAcceptedMsg
        stay using Partner(sender, voice, bg)
      } else {
        sender ! BusyMsg
        stay
      }
    }
    case Event(PairPreemptMsg(voice), Partner(oldPartner, _, bg)) => {
      if (oldPartner != unpaired) {
        oldPartner ! PreemptionDisconnectMsg
      }
      sender ! PairAcceptedMsg
      stay using Partner(sender, voice, bg)
    }
    case Event(UnpairMsg, Partner(partner, _, bg)) => {
      if (sender == partner) {
        stay using Partner(unpaired, VOICE_NONE, bg)
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerUtteranceMsg(utterance), Partner(partner, voice, _)) => {
      if (sender == partner) {
        log.info("Say '" + utterance + "' using voice " + voice)
        if (first) {
          say("Hello!", voice, false)
          first = false
        }
        say(utterance, voice, true)
        partner ! SpeakerSoundFinishedMsg
        stay
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerListenMsg(newPersonName), Partner(partner, _, _)) => {
      if (sender == partner) {
        log.info("Listening...")
        if (isWatsonEnabled) {
          val cl = classOf[javax.sound.sampled.AudioSystem].getClassLoader
          val old = Thread.currentThread.getContextClassLoader
          try {
            Thread.currentThread.setContextClassLoader(cl)
            listen(partner, newPersonName)
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
    case Event(RingtoneMsg, Partner(partner, _, _)) => {
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
    case Event(DoorbellMsg, Partner(partner, _, _)) => {
      log.info("Ding dong")
      val command = settings.Speaker.doorbellCommand
      if (!command.isEmpty) {
        command.!
      }
      sender ! SpeakerSoundFinishedMsg
      stay
    }
    case Event(StartAudioFileMsg(file, loop),
      Partner(partner, voice, background)) =>
    {
      background match {
        case Some(process) => {
          process.destroy
        }
        case _ =>
      }
      val command = {
        if (loop) {
          settings.Speaker.loopFileCommand
        } else {
          settings.Speaker.playFileCommand
        }
      }
      val process = command.format(file).run
      stay using Partner(partner, voice, Some(process))
    }
    case Event(StopAudioFileMsg, Partner(partner, voice, background)) => {
      background match {
        case Some(process) => {
          process.destroy
          stay using Partner(partner, voice)
        }
        case _ => {
          stay
        }
      }
    }
  }

  private def isWatsonEnabled =
    !settings.WatsonTts.user.isEmpty && !settings.WatsonStt.user.isEmpty

  private def say(utterance : String, voice : String, cache : Boolean)
  {
    if (!isWatsonEnabled) {
      return
    }
    val file = new File(
      audioDir, "tts-" + URLEncoder.encode(utterance, "UTF-8") + ".wav")
    if (cache && file.isFile) {
      (settings.Speaker.command #< file).!
    } else {
      val stream = tts.synthesize(
        utterance, Voice.EN_ALLISON, WatsonAudioFormat.WAV)
      val in = WaveUtils.reWriteWaveHeader(stream.execute)
      ((("tee " + file) #| settings.Speaker.command) #< in).!
    }
  }

  private def listen(partner : ActorRef, newPersonName : String)
  {
    var result : AnyRef = SilenceMsg
    val sampleRate = 44100
    val format = new JavaAudioFormat(sampleRate, 16, 1, true, true)
    val info = new DataLine.Info(classOf[TargetDataLine], format)
    val line = AudioSystem.getLine(info).asInstanceOf[TargetDataLine]
    line.open(format)
    line.start
    try {
      val orig = new AudioInputStream(line)
      val pipedOutputStream = new PipedOutputStream
      val pipedInputStream = new BufferedInputStream(
        new PipedInputStream(pipedOutputStream))
      val rawFile = File.createTempFile("stt-", ".au", audioDir)
      val wavFile = new File(
        rawFile.getCanonicalPath.stripSuffix(".au") + ".wav")
      val teeOutputStream = new TeeOutputStream(
        pipedOutputStream, new FileOutputStream(rawFile))
      val pipeFuture = Future {
        AudioSystem.write(orig, AudioFileFormat.Type.AU, teeOutputStream)
      }(ExecutionContext.Implicits.global)
      val audio = AudioSystem.getAudioInputStream(pipedInputStream)
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
            result = PersonUtteranceMsg(utterance, newPersonName)
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
      Await.ready(pipeFuture, Duration.Inf)
      teeOutputStream.close
      (s"sox $rawFile $wavFile").!
      rawFile.delete
      if (newPersonName.isEmpty) {
        if (personCount > 0) {
          val recognitoResults = recognito.identify(wavFile)
          if (!recognitoResults.isEmpty) {
            val identifiedPersonName = recognitoResults.get(0).getKey
            result match {
              case PersonUtteranceMsg(utterance, _) => {
                result = PersonUtteranceMsg(utterance, identifiedPersonName)
              }
              case _ =>
            }
          }
        }
      } else {
        recognito.createVoicePrint(newPersonName, wavFile)
        personCount += 1
      }
    } finally {
      line.stop
      line.close
      log.info("Done listening.")
      partner ! result
    }
  }

  initialize()
}
