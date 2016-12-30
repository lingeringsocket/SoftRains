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

import scala.concurrent._
import scala.io._

import com.typesafe.config._

object KioskApp extends App
{
  val config = ConfigFactory.load
  val settings = SoftRainsSettings(config)
  val system = ActorSystem("SoftRainsIntercom", config)
  val cameraSpec = settings.Actors.camera
  assert (!cameraSpec.isEmpty)
  val intercomSpec = settings.Actors.intercom
  assert (!intercomSpec.isEmpty)
  val intercomActor =
    system.actorOf(Props(classOf[IntercomActor]), intercomSpec)
  val cameraActor =
    system.actorOf(Props(classOf[CameraActor]), cameraSpec)
  val detectedActor =
    system.actorOf(Props(classOf[FaceDetectedUrlActor]), "faceDetectedActor")
  // FIXME:  make configurable
  val input = new CameraFeedInput("http://127.0.0.1:8081")
  val view = CameraNullView
  cameraActor.tell(CameraActor.StartSentinelMsg(input, view), detectedActor)
  println("Akka listening, press RETURN to stop...")
  StdIn.readLine
  cameraActor ! CameraActor.StopSentinelMsg
  system.terminate
  Await.result(system.whenTerminated, duration.Duration.Inf)
}

class FaceDetectedUrlActor extends Actor
{
  private val settings = SoftRainsActorSettings(context)

  def receive =
  {
    case CameraActor.FaceDetectedMsg => {
      val stateUrl = settings.Openhab.url + "/rest/items/facetime/state"
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.putString(stateUrl, "ON") {}
      httpConsumer.ensureSuccess
    }
  }
}
