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
    val input = new CameraFeedInput(settings.Kiosk.cameraUrl)
    val view = CameraNullView
    cameraActor ! CameraActor.StartSentinelMsg(input, view)
  }

  override def postStop()
  {
    cameraActor ! CameraActor.StopSentinelMsg
  }

  def receive =
  {
    case CameraActor.FaceDetectedMsg(name, confidence) => {
      val httpConsumer1 = new HttpConsumer(context.system)
      httpConsumer1.putString(faceNameUrl, name) {}
      httpConsumer1.ensureSuccess

      // only trigger facetime after the face name has been updated
      val httpConsumer2 = new HttpConsumer(context.system)
      httpConsumer2.putString(modeUrl, "ON") {}
      httpConsumer2.ensureSuccess
    }
    case IntercomActor.UnpairedMsg => {
      context.system.scheduler.scheduleOnce(
        10.seconds, cameraActor,
        CameraActor.ControlFaceDetectionMsg(true))
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "OFF") {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.ListeningStartedMsg => {
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "LISTENING") {}
      httpConsumer.ensureSuccess
    }
    case IntercomActor.ListeningDoneMsg => {
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(modeUrl, "ON") {}
      httpConsumer.ensureSuccess
    }
  }
}
