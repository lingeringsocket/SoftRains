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
package softrains.kiosk

import softrains.base._
import softrains.vision._
import softrains.intercom._

import akka.actor._
import akka.event._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object KioskActor
{
  val PRIMARY_INTERCOM_NAME = "kiosk"
}

class KioskActor(
  faceExampleLoader : Option[FaceExampleLoader],
  startLocalIntercom : Boolean)
    extends Actor with IntercomClient
{
  import KioskActor._

  private val log = Logging(context.system, this)

  private val settings = SoftRainsActorSettings(context)

  private val modeUrl =
    settings.Openhab.url + "/rest/items/facetime_mode/state"

  private val conversationUrl =
    "http://" + settings.Http.address + ":" + settings.Http.port + "/professor"

  private val faceNameUrl =
    settings.Openhab.url + "/rest/items/face_name/state"

  private val cameraSpec = settings.Actors.camera

  private val cameraActor = {
    if (cameraSpec.isEmpty) {
      None
    } else {
      Some(context.actorOf(Props(classOf[CameraActor]), cameraSpec))
    }
  }

  override def getActorSystem = context.system

  override def getSettings = settings

  override def preStart()
  {
    val cameraUrl = settings.Kiosk.cameraUrl
    val cameraWindowTitle = settings.Kiosk.cameraWindowTitle
    if (!cameraWindowTitle.isEmpty) {
      val input = {
        if (cameraUrl.isEmpty) {
          new CameraLocalInput
        } else {
          new CameraFeedInput(cameraUrl)
        }
      }
      val view = {
        if (cameraWindowTitle.isEmpty) {
          CameraNullView
        } else {
          new CameraDesktopView(cameraWindowTitle)
        }
      }
      cameraActor.foreach(
        _ ! CameraActor.StartSentinelMsg(input, view, faceExampleLoader))
    }
    if (startLocalIntercom) {
      startLocalIntercoms
    }
    val intercomActor = getIntercomActor
    intercomActor ! IntercomActor.SetObserverMsg(self)
    log.info("KioskActor started")
  }

  private def getIntercomActor = accessIntercomActor(PRIMARY_INTERCOM_NAME)

  override def postStop()
  {
    log.info("KioskActor stopped")
  }

  private def maybeNotify[T](eval : => T) =
  {
    if (!settings.Openhab.url.isEmpty) {
      eval
    }
  }

  def receive =
  {
    case IntercomActor.ReadyMsg(name) => {
      if (name == PRIMARY_INTERCOM_NAME) {
        val intercomActor = getIntercomActor
        intercomActor ! IntercomActor.SetObserverMsg(self)
      }
    }
    case faceMsg : CameraActor.FaceDetectedMsg => {
      log.info("KioskActor detected face " + faceMsg.name +
        " with confidence " + faceMsg.confidence)
      context.parent ! faceMsg
      val intercomActor = getIntercomActor
      intercomActor ! IntercomActor.SetObserverMsg(self)
      intercomActor ! IntercomActor.PreWakeMsg
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(faceNameUrl, faceMsg.name) {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.PairedMsg => {
      log.info("KioskActor paired")
      cameraActor.foreach(_ ! CameraActor.ControlFaceDetectionMsg(false))
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "ON") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.UnpairedMsg => {
      log.info("KioskActor unpaired")
      cameraActor.foreach(context.system.scheduler.scheduleOnce(
        10.seconds, _,
        CameraActor.ControlFaceDetectionMsg(true)))
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "OFF") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.ListeningStartedMsg => {
      log.info("KioskActor listening started")
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "LISTENING") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.ListeningDoneMsg => {
      log.info("KioskActor listening done")
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "ON") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.ConversationRequestedMsg => {
      log.info("KioskActor starting conversation")
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.fetchString(conversationUrl) {result => }
      httpConsumer.ensureSuccess
    }
    case initializeAlexaMsg : IntercomActor.InitializeAlexaMsg => {
      val intercomActor = getIntercomActor
      intercomActor ! initializeAlexaMsg
    }
  }
}
