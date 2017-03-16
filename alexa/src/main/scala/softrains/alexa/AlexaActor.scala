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
package softrains.alexa

import softrains.base._
import softrains.intercom._

import com.amazon.alexa.avs._
import com.amazon.alexa.avs.auth._
import com.amazon.alexa.avs.auth.companionservice._
import com.amazon.alexa.avs.config._
import com.amazon.alexa.avs.http._
import com.amazon.alexa.avs.message.response.speechsynthesizer._
import com.amazon.alexa.avs.wakeword._

import org.eclipse.jetty.util.ssl._

import akka.actor._
import akka.event._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object AlexaActor
{
  // received messages
  case object StartSessionMsg
      extends SoftRainsMsg
  // IntercomActor.WakeAlexaMsg
  case object ExpiryMsg
      extends SoftRainsMsg
}
import AlexaActor._

class AlexaActor(intercomActor : ActorRef) extends Actor
    with ExpectSpeechListener
    with RecordingRMSListener
    with RegCodeDisplayHandler
    with AccessTokenListener
    with ExpectStopCaptureListener
    with WakeWordDetectedHandler
    with AVSAudioPlayer.AlexaSpeechListener
{
  private val log = Logging(context.system, this)

  private val settings = SoftRainsActorSettings(context)

  private val deviceConfig = DeviceConfigUtils.readConfigFile(
    settings.Alexa.confFile.getPath)

  private val authSetup = new AuthSetup(deviceConfig, this)

  private var maybeStop = false

  private var expiryCancellable : Option[Cancellable] = None

  private var intercomOff = true

  private val clientFactory = new ClientFactory(deviceConfig)

  private val playerFactory = new PlayerFactory

  private val controller =
    new AVSController(
      this, playerFactory, new AlertManagerFactory,
      clientFactory,
      DialogRequestIdAuthority.getInstance,
      deviceConfig.getWakeWordAgentEnabled, new WakeWordIPCFactory,
      this)

  override def preStart()
  {
    log.info("AlexaActor started")
  }

  override def postStop()
  {
    playerFactory.playerOpt.foreach(_.stop)
    clientFactory.clientOpt.foreach(_.shutdown)
    log.info("AlexaActor stopped")
  }

  private def finishProcessing()
  {
    controller.processingFinished
  }

  override def displayRegCode(regCode : String)
  {
    val regUrl =
      deviceConfig.getCompanionServiceInfo.getServiceUrl + "/provision/" +
        regCode
    log.info(regUrl)
  }

  override def onAccessTokenReceived(accessToken : String)
  {
    log.info("TOKEN = " + accessToken)
  }

  override def onExpectSpeechDirective()
  {
  }

  override def onWakeWordDetected()
  {
    log.info("WAKE UP!!!!!")
    intercomActor ! IntercomActor.WakeAlexaMsg
  }

  override def rmsChanged(rms : Int)
  {
  }

  override def onStopCaptureDirective()
  {
    if (isIntercomOff) {
      return
    }
    intercomActor ! IntercomActor.ListeningDoneMsg
    log.info("STOP CAPTURE")
    controller.onUserActivity
    controller.stopRecording
    maybeStop = true
    expiryCancellable.foreach(_.cancel)
    expiryCancellable = Some(
      context.system.scheduler.scheduleOnce(5.seconds) {
        self ! ExpiryMsg
      }
    )
  }

  override def onAlexaSpeechStarted()
  {
    if (isIntercomOff) {
      return
    }
    expiryCancellable.foreach(_.cancel)
    expiryCancellable = None
    maybeStop = false
  }

  override def onAlexaSpeechFinished()
  {
    if (isIntercomOff) {
      return
    }
    startCapture
  }

  private def startCapture()
  {
    controller.onUserActivity
    val requestListener = new RequestListener {
      override def onRequestSuccess()
      {
        finishProcessing
      }

      override def onRequestError(e : Throwable)
      {
        e.printStackTrace
        log.error("An error occurred creating speech request: {}", e)
        finishProcessing
      }
    }
    val rmsListener = this
    log.info("START CAPTURE")
    intercomActor ! IntercomActor.ListeningStartedMsg
    controller.startRecording(rmsListener, requestListener)
  }

  def receive =
  {
    case StartSessionMsg => {
      authSetup.addAccessTokenListener(this)
      authSetup.addAccessTokenListener(controller)
      authSetup.startProvisioningThread
      controller.initializeStopCaptureHandler(this)
      controller.startHandlingDirectives
      playerFactory.playerOpt.foreach(
        _.registerAlexaSpeechListener(this))
    }

    case IntercomActor.WakeAlexaMsg => {
      intercomOff = false
      startCapture
    }
    case ExpiryMsg => {
      if (maybeStop) {
        intercomActor ! IntercomActor.AlexaFinishedMsg
        intercomOff = true
        maybeStop = false
      }
    }
  }

  private def isIntercomOff() = intercomOff

  class ClientFactory(config : DeviceConfig)
      extends AVSClientFactory(config)
  {
    var clientOpt : Option[AVSClient] = None

    override def getAVSClient(
      directiveEnqueuer : DirectiveEnqueuer,
      parsingFailedHandler : ParsingFailedHandler) =
    {
      val client = new AVSClient(
        config.getAvsHost,
        directiveEnqueuer,
        new SslContextFactory,
        parsingFailedHandler)
      clientOpt = Some(client)
      client
    }
  }

  class PlayerFactory extends AVSAudioPlayerFactory
  {
    var playerOpt : Option[AVSAudioPlayer] = None

    override def getAudioPlayer(controller : AVSController) =
    {
      val player = new AVSAudioPlayer(controller) {
        override def handleSpeak(speak : Speak)
        {
          if (!isIntercomOff) {
            super.handleSpeak(speak)
          }
        }
      }
      playerOpt = Some(player)
      player
    }
  }
}
