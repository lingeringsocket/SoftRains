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

import org.specs2.mutable._
import org.specs2.specification.core._

import java.io._

class CameraSentinelSpec extends Specification
{
  "CameraSentinel" should
  {
    "detect faces" >> {
      Fragment.foreach(
        Seq(
          ("data/johnLeaving.mkv", 0),
          ("data/johnArriving.mkv", 5),
          ("data/slowCar.mkv", 0)))
      {
        case (fileName, expectedCount) =>
          "in file " + fileName >> {
            {
              val input = new CameraFileInput(new File(fileName))
              val sentinel = new CameraSentinel(input, CameraNullView)
              sentinel.enableFaceDetection
              sentinel.run
              sentinel.getFaceFrameCount must be equalTo expectedCount
            }
          }
      }
    }
  }
}
