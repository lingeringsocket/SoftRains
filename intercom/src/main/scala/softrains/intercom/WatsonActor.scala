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
package softrains.intercom

import softrains.base._

import java.io._
import java.net._

import org.apache.commons.io.output._

import akka.actor._
import akka.event._

import com.ibm.watson.developer_cloud.text_to_speech.v1._
import com.ibm.watson.developer_cloud.text_to_speech.v1.model._
import com.ibm.watson.developer_cloud.text_to_speech.v1.util._

import com.ibm.watson.developer_cloud.speech_to_text.v1._
import com.ibm.watson.developer_cloud.speech_to_text.v1.model._
import com.ibm.watson.developer_cloud.speech_to_text.v1.websocket._

import com.ibm.watson.developer_cloud.http._

import javax.sound.sampled._

import com.bitsinharmony.recognito._

import scala.collection.JavaConverters._

import scala.concurrent._
import scala.concurrent.duration._

import scala.sys.process._

import com.ibm.watson.developer_cloud.text_to_speech.v1.model.{
  AudioFormat => WatsonAudioFormat
}
import javax.sound.sampled.{
  AudioFormat => JavaAudioFormat
}

object WatsonActor
{
  // received messages
  final case class SpeechListenMsg(
    personName : String = "",
    identifyVoice : Boolean = false)
      extends SoftRainsMsg
  final case class SpeechSayMsg(
    utterance : String, voice : String)
      extends SoftRainsMsg

  // sent messages include
  // IntercomActor.PersonUtteranceMsg
  // IntercomActor.SilenceMsg
  // IntercomActor.SpeakerSoundFinishedMsg
}
import WatsonActor._

class WatsonActor extends Actor
{
  private val log = Logging(context.system, this)

  private val settings = SoftRainsActorSettings(context)

  private val tts = new TextToSpeech

  private val stt = new SpeechToText

  private val audioDir = settings.Files.audioPath

  private val recognito = new Recognito[String](44100.0f)

  private var personCount = 0

  private var first = true

  override def preStart()
  {
    tts.setUsernameAndPassword(
      settings.WatsonTts.user, settings.WatsonTts.password)
    stt.setUsernameAndPassword(
      settings.WatsonStt.user, settings.WatsonStt.password)
    if (!audioDir.isDirectory) {
      if (!audioDir.mkdirs) {
        throw new IOException(
          "Unable to create audio directory " + audioDir)
      }
    }
  }

  def receive =
  {
    case SpeechListenMsg(personName, identifyVoice) => {
      listen(personName, identifyVoice)
    }
    case SpeechSayMsg(utterance, voice) => {
      log.info("Say '" + utterance + "' using voice " + voice)
      var file : Option[File] = None
      try {
        file = Some(say(utterance, voice, !first))
        first = false
      } finally {
        sender ! IntercomActor.SpeakerSoundFinishedMsg(
          file.map(_.getAbsolutePath))
      }
      log.info("Done speaking")
    }
  }

  private def say(utterance : String, voice : String, cache : Boolean) =
  {
    val file = new File(
      audioDir,
      "tts-" + voice +
        "-" + URLEncoder.encode(utterance, "UTF-8") + ".wav")
    if (cache && file.isFile) {
      (settings.Speaker.command #< file).!
    } else {
      val stream = tts.synthesize(
        utterance, Voice.getByName(voice), WatsonAudioFormat.WAV)
      val in = WaveUtils.reWriteWaveHeader(stream.execute)
      ((("tee " + file) #| settings.Speaker.command) #< in).!
    }
    file
  }

  private def listen(personName : String, identifyVoice : Boolean)
  {
    log.info("Listening...")
    var result : AnyRef = IntercomActor.SilenceMsg
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
        inactivityTimeout(30).
        maxAlternatives(3).build
      val speechPromise = Promise[SpeechResults]()
      val speechFuture = speechPromise.future
      val disconnectPromise = Promise[Object]()
      val disconnectFuture = disconnectPromise.future
      stt.recognizeUsingWebSocket(
        audio, options, new BaseRecognizeCallback {
          override def onTranscription(speechResults : SpeechResults)
          {
            audio.close
            try {
              val transcript =
                speechResults.getResults.get(speechResults.getResultIndex)
              val alternatives = transcript.getAlternatives.asScala.
                map(_.getTranscript.trim).toSeq
              log.info("Heard:  " + alternatives.head)
              result = IntercomActor.PersonUtteranceMsg(
                alternatives, personName, Some(wavFile.getAbsolutePath))
              speechPromise.success(speechResults)
            } catch {
              case ex : Throwable => {
                speechPromise.failure(ex)
              }
            }
          }

          override def onError(e : Exception)
          {
            audio.close
            // FIXME proper error handling
            speechPromise.failure(e)
          }

          override def onDisconnected()
          {
            speechPromise.tryFailure(new TimeoutException)
            disconnectPromise.success(null)
          }
        })
      val duration = FiniteDuration(30, java.util.concurrent.TimeUnit.SECONDS)
      var timeout = false
      try {
        Await.ready(disconnectFuture, duration)
        Await.ready(speechFuture, duration)
        Await.ready(pipeFuture, duration)
      } catch {
        case ex : TimeoutException => {
          timeout = true
        }
      }
      teeOutputStream.close
      (s"sox $rawFile $wavFile").!
      rawFile.delete
      if (!timeout) {
        if (identifyVoice) {
          if (personName.isEmpty) {
            if (personCount > 0) {
              val recognitoResults = recognito.identify(wavFile)
              if (!recognitoResults.isEmpty) {
                val identifiedPersonName = recognitoResults.get(0).getKey
                result match {
                  case IntercomActor.PersonUtteranceMsg(alternatives, _, _) => {
                    result = IntercomActor.PersonUtteranceMsg(
                      alternatives, identifiedPersonName,
                      Some(wavFile.getAbsolutePath))
                  }
                  case _ =>
                }
              }
            }
          } else {
            recognito.createVoicePrint(personName, wavFile)
            personCount += 1
          }
        }
      }
    } finally {
      line.stop
      line.close
      log.info("Done listening.")
      sender ! result
    }
  }
}
