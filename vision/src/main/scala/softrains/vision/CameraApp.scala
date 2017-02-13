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
package softrains.vision

import softrains.base._

import akka.actor._

import scala.concurrent._
import scala.io._

import com.typesafe.config._

object CameraApp extends App
{
  val config = ConfigFactory.load("localcam")
  val settings = SoftRainsSettings(config)
  val system = ActorSystem("SoftRainsCamera", config)
  val cameraSpec = settings.Actors.camera
  assert (!cameraSpec.isEmpty)
  val cameraActor =
    system.actorOf(Props(classOf[CameraActor]), cameraSpec)
  val detectedActor =
    system.actorOf(Props(classOf[FaceDetectedActor]), "faceDetectedActor")
  val input = new CameraLocalInput
  val view = new CameraDesktopView("Webcam")
  cameraActor.tell(CameraActor.StartSentinelMsg(input, view), detectedActor)
  println("Akka listening, press RETURN to stop...")
  StdIn.readLine
  cameraActor ! CameraActor.StopSentinelMsg
  system.terminate
  Await.result(system.whenTerminated, duration.Duration.Inf)
}

class FaceDetectedActor extends Actor
{
  def receive =
  {
    case msg : CameraActor.FaceDetectedMsg => {
      println("Face " + msg.name + " detected with confidence " +
        msg.confidence)
    }
  }
}
