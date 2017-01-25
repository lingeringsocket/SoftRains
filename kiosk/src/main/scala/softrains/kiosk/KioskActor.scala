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

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class KioskActor extends Actor
{
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
    context.actorOf(Props(classOf[IntercomActor]), intercomSpec)

  private val cameraActor =
    context.actorOf(Props(classOf[CameraActor]), cameraSpec)

  override def preStart()
  {
    val cameraUrl = settings.Kiosk.cameraUrl
    if (!cameraUrl.isEmpty) {
      val input = new CameraFeedInput(cameraUrl)
      val view = CameraNullView
      cameraActor ! CameraActor.StartSentinelMsg(input, view)
    }
  }

  override def postStop()
  {
    cameraActor ! CameraActor.StopSentinelMsg
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
      intercomActor ! IntercomActor.PreWakeMsg
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.postString(faceNameUrl, name) {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.PairedMsg => {
      cameraActor ! CameraActor.ControlFaceDetectionMsg(false)
      maybeNotify {
        val httpConsumer = new HttpConsumer(context.system)
        httpConsumer.putString(modeUrl, "ON") {}
        httpConsumer.ensureSuccess
      }
    }
    case IntercomActor.UnpairedMsg => {
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
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "LISTENING") {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.ListeningDoneMsg => maybeNotify {
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "ON") {}
      httpConsumer.ensureSuccess
    }
    case initializeAlexaMsg : IntercomActor.InitializeAlexaMsg => {
      intercomActor ! initializeAlexaMsg
    }
  }
}
