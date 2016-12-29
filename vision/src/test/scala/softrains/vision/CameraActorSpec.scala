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

import scala.concurrent.duration._

class CameraActorSpec extends AkkaActorSpecification
{
  import CameraActor._

  private def getVideoFile(resource : String) =
    getResourceFile("/video/" + resource)

  "CameraActor" should
  {
    "detect face" in new AkkaActorExample
    {
      val timeout = 10.seconds
      val input = new CameraFileInput(getVideoFile("johnArriving.mkv"))
      val actor = system.actorOf(Props(classOf[CameraActor]))
      actor ! StartSentinelMsg(input, CameraNullView, FaceDetectedMsg)
      expectMsg(timeout, FaceDetectedMsg)
      actor ! StopSentinelMsg
      fishForMessage(timeout) {
        msg : Any => msg match {
          case SentinelStoppedMsg => true
          case _ => false
        }
      }
    }
  }
}
