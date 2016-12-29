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

import scala.concurrent.ExecutionContext.Implicits.global

object CameraActor
{
  sealed trait State
  sealed trait Data

  // received messages
  final case class StartSentinelMsg(
    input : CameraInput,
    view : CameraView,
    faceDetectedMsg : SoftRainsMsg = FaceDetectedMsg) extends SoftRainsMsg
  case object AnalyzeFrameMsg extends SoftRainsMsg
  case object StopSentinelMsg extends SoftRainsMsg

  // sent messages
  // dynamic based on input to StartSentinelMsg
  case object FaceDetectedMsg extends SoftRainsMsg
  case object SentinelStoppedMsg extends SoftRainsMsg

  case object Inactive extends State
  case object Active extends State

  case object Empty extends Data
  final case class SentinelData(
    sentinel : CameraSentinel,
    faceDetectedMsg : SoftRainsMsg,
    listener : ActorRef,
    cancellable : Cancellable) extends Data
}
import CameraActor._

class CameraActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val frameInterval = settings.Visitors.frameInterval

  startWith(Inactive, Empty)

  when(Inactive) {
    case Event(StartSentinelMsg(input, view, faceDetectedMsg), _) => {
      val sentinel = new CameraSentinel(input, view, settings)
      sentinel.enableFaceDetection(false)
      sentinel.startAnalyzer
      val cancellable = context.system.scheduler.schedule(
        frameInterval, frameInterval,
        self, AnalyzeFrameMsg)
      goto(Active) using SentinelData(
        sentinel, faceDetectedMsg, sender, cancellable)
    }
    case Event(AnalyzeFrameMsg, _) => {
      stay
    }
  }

  when(Active) {
    case Event(AnalyzeFrameMsg,
      SentinelData(sentinel, faceDetectedMsg, listener, _)) =>
    {
      sentinel.analyzeFrame
      if (sentinel.wasFaceDetected) {
        listener ! faceDetectedMsg
      }
      stay
    }
    case Event(StopSentinelMsg, SentinelData(sentinel, _, _, cancellable)) => {
      cancellable.cancel
      sentinel.stopAnalyzer
      sender ! SentinelStoppedMsg
      goto(Inactive) using Empty
    }
  }

  initialize()
}
