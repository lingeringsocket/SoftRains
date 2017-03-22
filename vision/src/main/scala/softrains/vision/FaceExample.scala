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

import java.io._

case class FaceExample(
  label : String,
  imgFile : File
)

trait FaceExampleLoader
{
  def load() : Seq[FaceExample]
}

class FaceExampleDirectory(settings : SoftRainsSettings)
    extends FaceExampleLoader
{
  override def load() : Seq[FaceExample] =
  {
    if (settings.Visitors.trainingPathString.isEmpty) {
      return Seq.empty
    }
    val root = settings.Visitors.trainingPath
    val imgFilter = new FilenameFilter {
      override def accept(dir : File, nameOrig : String) =
      {
        val name = nameOrig.toLowerCase
        name.endsWith(".jpg") || name.endsWith(".pgm") || name.endsWith(".png")
      }
    }

    val imageFiles = root.listFiles(imgFilter)
    imageFiles.map(file => FaceExample(
      file.getName.split("\\-")(0),
      file))
  }
}
