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
package softrains.kiosk

import softrains.base._
import softrains.vision._
import softrains.intercom._

import akka.actor._
import akka.event._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class KioskActor extends Actor
{
  private val log = Logging(context.system, this)

  private val settings = SoftRainsActorSettings(context)

  private val modeUrl =
    settings.Openhab.url + "/rest/items/facetime_mode/state"

  private val faceNameUrl =
    settings.Openhab.url + "/rest/items/face_name/state"

  private val cameraSpec = settings.Actors.camera
  assert (!cameraSpec.isEmpty)

  private val intercomSpec = settings.Actors.intercom
  assert (!intercomSpec.isEmpty)

  private val intercomActor =
  {
    if (intercomSpec.startsWith("akka:")) {
      // FIXME share code with CentralService, and make sure
      // we never start two different IntercomActors
      // at once!
      val intercomActorSelection =
        context.system.actorSelection(intercomSpec)
      val intercomActorTimeout =
        FiniteDuration(
          10, java.util.concurrent.TimeUnit.SECONDS)
      val intercomActorFuture = intercomActorSelection.resolveOne(
        intercomActorTimeout)
      Await.result(
        intercomActorFuture, intercomActorTimeout)
    } else {
      context.actorOf(Props(classOf[IntercomActor]), intercomSpec)
    }
  }

  private val cameraActor =
    context.actorOf(Props(classOf[CameraActor]), cameraSpec)

  override def preStart()
  {
    val cameraUrl = settings.Kiosk.cameraUrl
    val cameraWindowTitle = settings.Kiosk.cameraWindowTitle
    if (!cameraUrl.isEmpty) {
      val input = new CameraFeedInput(cameraUrl)
      val view = {
        if (cameraWindowTitle.isEmpty) {
          CameraNullView
        } else {
          new CameraDesktopView(cameraWindowTitle)
        }
      }
      cameraActor ! CameraActor.StartSentinelMsg(input, view)
    }
    intercomActor ! IntercomActor.SetObserverMsg(self)
    log.info("KioskActor started")
  }

  override def postStop()
  {
    cameraActor ! CameraActor.StopSentinelMsg
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
    case CameraActor.FaceDetectedMsg(name, confidence) => maybeNotify {
      log.info("KioskActor detected face " + name + " with confidence " +
        confidence)
      intercomActor ! IntercomActor.PreWakeMsg
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(faceNameUrl, name) {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.PairedMsg => {
      log.info("KioskActor paired")
      cameraActor ! CameraActor.ControlFaceDetectionMsg(false)
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "ON") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.UnpairedMsg => {
      log.info("KioskActor unpaired")
      context.system.scheduler.scheduleOnce(
        10.seconds, cameraActor,
        CameraActor.ControlFaceDetectionMsg(true))
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "OFF") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.ListeningStartedMsg => maybeNotify {
      log.info("KioskActor listening started")
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "LISTENING") {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.ListeningDoneMsg => maybeNotify {
      log.info("KioskActor listening done")
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "ON") {}
      httpConsumer.ensureSuccess
    }
    case initializeAlexaMsg : IntercomActor.InitializeAlexaMsg => {
      intercomActor ! initializeAlexaMsg
    }
  }
}
