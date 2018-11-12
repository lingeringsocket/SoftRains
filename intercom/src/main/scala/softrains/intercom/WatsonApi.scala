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

import com.typesafe.config._

import com.ibm.watson.developer_cloud.service.security._

import com.ibm.watson.developer_cloud.text_to_speech.v1._
import com.ibm.watson.developer_cloud.text_to_speech.v1.model._
import com.ibm.watson.developer_cloud.text_to_speech.v1.util._

import com.ibm.watson.developer_cloud.http._

import com.ibm.watson.developer_cloud.speech_to_text.v1._
import com.ibm.watson.developer_cloud.speech_to_text.v1.model._
import com.ibm.watson.developer_cloud.speech_to_text.v1.websocket._

import javax.sound.sampled._

import scala.collection.JavaConverters._

import scala.concurrent._
import scala.concurrent.duration._

import scala.sys.process._

import java.io._

import javax.sound.sampled.{
  AudioFormat => JavaAudioFormat
}

class WatsonApi(settings : SoftRainsSettings)
{
  private val SAMPLE_RATE = 44100

  private val tts = initTts

  private val stt = initStt

  private def initTts() =
  {
    val options = new IamOptions.Builder().
      apiKey(settings.WatsonTts.apiKey).build
    new TextToSpeech(options)
  }

  private def initStt() =
  {
    val options = new IamOptions.Builder().
      apiKey(settings.WatsonStt.apiKey).build
    val sttNew = new SpeechToText(options)
    sttNew
  }

  def textToSpeech(utterance : String, voice : String) : InputStream =
  {
    val options = new SynthesizeOptions.Builder().
      text(utterance).voice(voice).
      accept(SynthesizeOptions.Accept.AUDIO_WAV).build
    val stream = tts.synthesize(options)
    WaveUtils.reWriteWaveHeader(stream.execute)
  }

  def openAudioLine() : TargetDataLine =
  {
    val format = new JavaAudioFormat(SAMPLE_RATE, 16, 1, true, true)
    val info = new DataLine.Info(classOf[TargetDataLine], format)
    val line = AudioSystem.getLine(info).asInstanceOf[TargetDataLine]
    line.open(format)
    line.start
    line
  }

  def closeAudioLine(line : TargetDataLine)
  {
    line.stop
    line.close
  }

  def speechToText(
    audioStream : InputStream,
    timeout : Duration,
    onSuccess : Seq[String] => Unit) =
  {
    val filteredStream = new SilenceDetectionInputStream(
      audioStream,
      () => {
        audioStream.close
      })
    val disconnectPromise = Promise[Object]()
    val disconnectFuture = disconnectPromise.future
    val options = (new RecognizeOptions.Builder).
      audio(filteredStream).
      contentType(HttpMediaType.AUDIO_RAW + "; rate=" + SAMPLE_RATE).
      maxAlternatives(3).build
    stt.recognizeUsingWebSocket(
      options, new BaseRecognizeCallback {
        override def onTranscription(speechResults : SpeechRecognitionResults)
        {
          val resultList = speechResults.getResults
          if (!resultList.isEmpty) {
            val transcript =
              resultList.get(speechResults.getResultIndex.toInt)
            if (transcript.isFinalResults) {
              audioStream.close
              try {
                val alternatives = transcript.getAlternatives.asScala.
                  map(_.getTranscript.trim).toSeq
                onSuccess(alternatives)
              } catch {
                case ex : Throwable => {
                  // treat as silence
                }
              }
              disconnectPromise.trySuccess(null)
            }
          }
        }

        override def onError(e : Exception)
        {
          // treat as silence
          audioStream.close
          disconnectPromise.trySuccess(null)
        }

        override def onDisconnected()
        {
          audioStream.close
          disconnectPromise.trySuccess(null)
        }
      }
    )
    try {
      Await.ready(disconnectFuture, timeout)
    } catch {
      case ex : TimeoutException => {
        // treat as silence
      }
    }
  }
}

object WatsonApiApp extends App
{
  private val config = ConfigFactory.load
  private val settings = SoftRainsSettings(config)
  private val api = new WatsonApi(settings)

  run()

  def run()
  {
    try {
      say("Polly wants a cracker.")
      while (true) {
        val echo = listen()
        say(echo)
        if (echo.toLowerCase.contains("bye")) {
          return
        }
      }
    } finally {
      cleanup
    }
  }

  def say(utterance : String)
  {
    val stream = api.textToSpeech(utterance, "en-GB_KateVoice")
    try {
      val command = (settings.Speaker.command #< stream)
      command.!
    } finally {
      stream.close
    }
  }

  def listen() : String =
  {
    println("Listening...")
    val line = api.openAudioLine
    try {
      val audioStream = new AudioInputStream(line)
      val timeout = FiniteDuration(30, java.util.concurrent.TimeUnit.SECONDS)
      var result = "Goodbye"
      api.speechToText(audioStream, timeout,
        alternatives => {
          result = alternatives.head
          println("Heard:  " + result)
        }
      )
      result
    } finally {
      api.closeAudioLine(line)
    }
  }

  def cleanup()
  {
    println("Goodbye.")
    val client = HttpClientSingleton.getInstance.configureClient(null)
    client.dispatcher.executorService.shutdown
    client.connectionPool.evictAll
  }
}

class SilenceDetectionInputStream(
  in : InputStream,
  onFinalSilence : () => Unit)
    extends FilterInputStream(in)
{
  private var mod = 0

  private var leftover = 0.toByte

  private var sinceLast = 0

  private var duration = 0

  private var noise = false

  private var speaking = false

  private var finalTimer = 0

  override def read() : Int =
  {
    val next = in.read
    if (next != -1) {
      process(next.toByte)
    }
    next
  }

  override def read(b : Array[Byte], off : Int, len : Int) : Int =
  {
    val filled = super.read(b, off, len)
    if (filled > 0) {
      b.view(off, off + filled).foreach(b => process(b))
    }
    filled
  }

  private def process(next : Byte)
  {
    if (mod == 0) {
      leftover = next
      mod = 1
    } else {
      val hi = leftover
      val lo = next
      val sample = ((hi.toShort << 8) | lo).toShort
      emit(sample)
      mod = 0
    }
  }

  private def emit(sample : Int)
  {
    val level = if (sample < 0) -sample else sample
    if (level > 12000) {
      if (noise) {
        if (!speaking && (duration > 10000)) {
          speaking = true
          finalTimer = 0
        }
        duration += 1
        sinceLast = 0
      } else {
        sinceLast = 0
        duration = 0
        noise = true
      }
    } else {
      if (noise) {
        sinceLast += 1
        if (sinceLast > 9000) {
          if (speaking) {
            speaking = false
            finalTimer = 100000
          }
          noise = false
          sinceLast = 0
          duration = 0
        } else {
          duration += 1
        }
      }
    }
    if (finalTimer == 1) {
      // cut the mike
      onFinalSilence()
    } else if (finalTimer > 0) {
      finalTimer -= 1
    }
  }
}
