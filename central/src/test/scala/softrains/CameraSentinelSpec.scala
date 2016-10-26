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
package softrains

import com.typesafe.config._

import org.specs2.mutable._
import org.specs2.specification.core._

import java.io._

class CameraSentinelSpec extends Specification
{
  private val settings = CentralSettings(ConfigFactory.load)

  "CameraSentinel" should
  {
    "detect faces" >> {
      Fragment.foreach(
        Seq(
          ("data/johnLeaving.mkv", 0, 2),
          ("data/rhiannonArriving.mkv", 0, 2),
          ("data/johnArriving.mkv", 4, 8),
          ("data/pedestrians.mkv", 0, 0),
          ("data/muniLeft.mkv", 0, 0),
          ("data/nightCar.mkv", 0, 0)))
      {
        case (fileName, faceCount, visitorCount) =>
          "in file " + fileName >> {
            {
              val input = new CameraFileInput(new File(fileName))
              val sentinel = new CameraSentinel(
                input, CameraNullView, settings)
              sentinel.enableVisitorDetection(false)
              sentinel.run
              sentinel.getFaceFrameCount must be equalTo faceCount
              sentinel.getVisitorFrameCount must be equalTo visitorCount
            }
          }
      }
    }
  }
}
