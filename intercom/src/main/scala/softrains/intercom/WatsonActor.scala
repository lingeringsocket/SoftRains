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

import javax.sound.sampled._

import com.bitsinharmony.recognito._

import scala.concurrent._
import scala.concurrent.duration._

import scala.sys.process._

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

  private val watsonApi = new WatsonApi(settings)

  private val audioDir = settings.Files.audioPath

  private val recognito = new Recognito[String](44100.0f)

  private var personCount = 0

  private var first = true

  override def preStart()
  {
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
      val in = watsonApi.textToSpeech(utterance, voice)
      ((("tee " + file) #| settings.Speaker.command) #< in).!
    }
    file
  }

  private def listen(personName : String, identifyVoice : Boolean)
  {
    log.info("Listening...")
    var result : AnyRef = IntercomActor.SilenceMsg
    val line = watsonApi.openAudioLine
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
      val timeout = FiniteDuration(30, java.util.concurrent.TimeUnit.SECONDS)
      watsonApi.speechToText(audio, timeout,
        alternatives => {
          log.info("Heard:  " + alternatives.head)
          result = IntercomActor.PersonUtteranceMsg(
            alternatives, personName, Some(wavFile.getAbsolutePath))
        }
      )
      try {
        Await.ready(pipeFuture, timeout)
      } catch {
        case ex : TimeoutException => {
          // treat as silence
        }
      }
      teeOutputStream.close
      (s"sox $rawFile $wavFile").!
      rawFile.delete
      if (result != IntercomActor.SilenceMsg) {
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
      watsonApi.closeAudioLine(line)
      log.info("Done listening.")
      sender ! result
    }
  }
}
