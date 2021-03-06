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
    faceExampleLoader : Option[FaceExampleLoader] = None) extends SoftRainsMsg
  case object AnalyzeFrameMsg extends SoftRainsMsg
  case object StopSentinelMsg extends SoftRainsMsg
  final case class ControlFaceDetectionMsg(enable : Boolean)
      extends SoftRainsMsg

  // sent messages
  final case class FaceDetectedMsg(
    name : String,
    confidence : Double,
    faceFile : String,
    sceneFile : String)
      extends SoftRainsMsg
  case object SentinelStoppedMsg extends SoftRainsMsg

  case object Inactive extends State
  case object Active extends State

  case object Empty extends Data
  final case class SentinelData(
    sentinel : CameraSentinel,
    listener : ActorRef) extends Data
}
import CameraActor._

class CameraActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val frameInterval = settings.Visitors.frameInterval

  startWith(Inactive, Empty)

  override def preStart()
  {
    log.info("CameraActor started")
  }

  override def postStop()
  {
    super.postStop
    log.info("CameraActor stopped")
  }

  when(Inactive) {
    case Event(StartSentinelMsg(input, view, loaderOpt), _) => {
      val sentinel = new CameraSentinel(input, view, settings)
      loaderOpt.foreach(sentinel.setFaceExampleLoader(_))
      sentinel.enableFaceDetection(true)
      sentinel.startAnalyzer
      self ! AnalyzeFrameMsg
      goto(Active) using SentinelData(
        sentinel, sender)
    }
    case Event(AnalyzeFrameMsg, _) => {
      stay
    }
  }

  when(Active) {
    case Event(AnalyzeFrameMsg,
      SentinelData(sentinel, listener)) =>
    {
      context.system.scheduler.scheduleOnce(
        frameInterval, self, AnalyzeFrameMsg)
      try {
        sentinel.analyzeFrame
        val face = sentinel.getLastFace
        if (sentinel.wasFaceDetected) {
          log.info("CameraActor detected face " + face)
          listener ! FaceDetectedMsg(
            face, sentinel.getFaceConfidence,
            sentinel.getFaceFile, sentinel.getSceneFile)
        }
      } catch {
        // keep the actor running even if we miss a frame
        case ex : Exception => {
          ex.printStackTrace
        }
      }
      stay
    }
    case Event(StopSentinelMsg, SentinelData(sentinel, _)) => {
      sentinel.stopAnalyzer
      sender ! SentinelStoppedMsg
      goto(Inactive) using Empty
    }
    case Event(ControlFaceDetectionMsg(enable),
      SentinelData(sentinel, _)) =>
    {
      if (enable) {
        sentinel.enableFaceDetection(true)
      } else {
        sentinel.disableFaceDetection
      }
      stay
    }
  }

  onTermination {
    case StopEvent(_, Active, SentinelData(sentinel, _)) => {
      log.info("CameraActor stopping while active")
      sentinel.stopAnalyzer
    }
  }

  initialize()
}
