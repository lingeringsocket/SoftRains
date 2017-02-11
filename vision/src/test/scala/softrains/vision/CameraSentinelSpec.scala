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

import com.typesafe.config._

import org.specs2.mutable._
import org.specs2.specification.core._

import java.io._
import java.nio.file._
import java.nio.file.attribute._

class CameraSentinelSpec extends Specification
{
  // filesystem state is shared, so we need isolation
  sequential

  private val settings = SoftRainsSettings(ConfigFactory.load("test.conf"))

  private def cleanVideoFiles()
  {
    val dir = settings.Files.videoPath
    if (dir.isDirectory) {
      Files.walkFileTree(dir.toPath, new SimpleFileVisitor[Path] {
        override def visitFile(
          file : Path, attrs : BasicFileAttributes) =
        {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(
          dir : Path, exc : IOException) =
        {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
  }

  private def getVideoFile(resource : String) =
    getResourceFile("/video/" + resource)

  "CameraSentinel" should
  {
    "detect faces" >> {
      Fragment.foreach(
        Seq(
          ("johnLeaving.mkv", false, true),
          ("johnArriving.mkv", true, true),
          ("rhiannonArriving.mkv", false, true),
          ("rhiannonLeaving.mkv", false, true),
          ("pedestrians.mkv", false, false),
          ("muniLeft.mkv", false, false),
          ("muniRight.mkv", false, false),
          ("nightCar.mkv", false, false)))
      {
        case (fileName, faceExpected, visitorExpected) =>
          "in file " + fileName >> {
            {
              val input = new VideoFileInput(getVideoFile(fileName))
              val sentinel = new CameraSentinel(
                input, CameraNullView, settings)
              sentinel.enableVisitorDetection
              sentinel.enableFaceDetection(false)
              sentinel.run
              sentinel.wasFaceDetected must be equalTo faceExpected
              sentinel.wasVisitorDetected must be equalTo visitorExpected
            }
          }
      }
    }

    "record video motion" in
    {
      cleanVideoFiles
      val dir = settings.Files.videoPath
      dir.isDirectory must beFalse
      val input = new VideoFileInput(getVideoFile("muniRight.mkv"))
      val sentinel = new CameraSentinel(
        input, CameraNullView, settings)
      sentinel.enableMotionRecording
      sentinel.enableFaceDetection(true)
      sentinel.enableVisitorDetection
      sentinel.run
      sentinel.wasVisitorDetected must be equalTo false
      sentinel.wasFaceDetected must be equalTo false
      sentinel.endRecording
      dir.isDirectory must beTrue
      val dirs = dir.list
      dirs must have size(2)
      val faces = "faces"
      dirs.filter(_ == faces) must have size(1)
      val dayDirName = dirs.filterNot(_ == faces).head
      val dayDir = new File(dir, dayDirName)
      val recordings = dayDir.list
      recordings must have size(1)
      recordings.filter(_.endsWith("mkv")) must have size(1)
      val faceDir = new File(dir, faces)
      faceDir.list must beEmpty
    }

    "record faces" in
    {
      cleanVideoFiles
      val dir = settings.Files.videoPath
      dir.isDirectory must beFalse
      val input = new VideoFileInput(getVideoFile("johnArriving.mkv"))
      val sentinel = new CameraSentinel(
        input, CameraNullView, settings)
      sentinel.enableMotionRecording
      sentinel.enableFaceDetection(true)
      sentinel.enableVisitorDetection
      sentinel.run
      sentinel.wasVisitorDetected must be equalTo true
      sentinel.wasFaceDetected must be equalTo true
      sentinel.endRecording
      dir.isDirectory must beTrue
      val dirs = dir.list
      dirs must have size(2)
      val faces = "faces"
      dirs.filter(_ == faces) must have size(1)
      val dayDirName = dirs.filterNot(_ == faces).head
      val dayDir = new File(dir, dayDirName)
      val recordings = dayDir.list
      recordings must have size(3)
      recordings.filter(_.endsWith("mkv")) must have size(3)
      val faceDir = new File(dir, faces)
      faceDir.list must have size(3)
    }

    "allow face detection to be enabled and disabled" in
    {
      val input = new VideoFileInput(getVideoFile("johnArriving.mkv"))
      val sentinel = new CameraSentinel(
        input, CameraNullView, settings)
      sentinel.enableFaceDetection(false)
      sentinel.startAnalyzer
      var faceCount = 0
      while (!input.isClosed) {
        sentinel.analyzeFrame
        if (sentinel.wasFaceDetected) {
          sentinel.getLastFace must be equalTo "john"
          sentinel.getFaceConfidence must be closeTo 106.0 +/- 2.0
          faceCount += 1
          sentinel.disableFaceDetection
        }
      }
      sentinel.stopAnalyzer
      faceCount must be equalTo 1
    }

    "do not detect ghost face even with pareidolia" in
    {
      val input = new VideoFileInput(getVideoFile("ghostface.mkv"))
      val sentinel = new CameraSentinel(
        input, CameraNullView, settings)
      sentinel.enableFaceDetection(false)
      sentinel.inducePareidolia
      sentinel.startAnalyzer
      try {
        sentinel.analyzeFrame
        sentinel.wasFaceDetected must beFalse
      } finally {
        sentinel.stopAnalyzer
      }
    }
  }
}
