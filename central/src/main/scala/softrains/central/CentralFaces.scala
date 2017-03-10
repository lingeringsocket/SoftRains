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
package softrains.central

import scala.xml._

class CentralFaces(central : CentralService)
{
  private val db = central.db

  def labelsPage() : NodeSeq =
  {
    <html><body><table>
    {
      db.query[ResidentAppearance].
        order("imageTime", true).
        fetch.
        map(appearance => {
          val sceneUrl = appearance.generateSceneUrl(central.getSettings)
          val faceUrl = appearance.generateFaceUrl(central.getSettings)
          <tr>
            <td>
              <img src={sceneUrl}/>
            </td>
            <td>
              <img src={faceUrl}/>
            </td>
            <td>
              {appearance.resident.name}
            </td>
          </tr>
        })
    }
    </table></body></html>
  }
}
