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
package softrains.central

import scala.xml._

class CentralFaces(central : CentralService)
{
  private val db = central.db

  private def getAppearanceName(residentOpt : Option[HomeResident]) =
  {
    residentOpt match {
      case Some(resident) => {
        resident.name
      }
      case _ => {
        "Guest"
      }
    }
  }

  def labelsPage(unreviewedOnly : Boolean) : NodeSeq =
  {
    <html><body><table>
    {
      val query = db.query[ResidentAppearance] |> (
        q => if (unreviewedOnly) q.whereEqual("reviewed", false) else q)
      query.
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
            <td><a href={"/faces/" + appearance.id}>{
              getAppearanceName(appearance.resident)
            }</a></td>
          </tr>
        })
    }
    </table></body></html>
  }

  def detailPage(id : Int) : NodeSeq =
  {
    val appearance = db.fetchById[ResidentAppearance](id)
    <html><body><table>
    {
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
          {getAppearanceName(appearance.resident)}
        </td>
        <td>
          <a href={"/faces/" + id + "/accept"}>Accept</a>
        </td>
        <td>
          <a href={"/faces/" + id + "/delete"}>Delete</a>
        </td>
        <td>
          <a href={"/faces/" + id + "/relabel/0"}>Guest</a>
        </td>
      </tr>
    }
    </table><table>
    {
      val query = db.query[HomeResident] |> (
        q => appearance.resident match {
          case Some(resident) => {
            q.whereNotEqual("name", resident.name)
          }
          case _ => {
            q
          }
        })
      query.
        fetch.
        map(resident => {
          <tr>
            <td>
              <a href={"/faces/" + id + "/relabel/" + resident.id}>
                {resident.name}</a>
            </td>
          </tr>
        })
    }
    </table></body></html>
  }

  def delete(id : Int) : NodeSeq =
  {
    db.delete(db.fetchById[ResidentAppearance](id))
    <html><body>
      Face appearance deleted.
      <a href="/faces/unreviewed">Return to label browser.</a>
    </body></html>
  }

  def accept(id : Int) : NodeSeq =
  {
    db.save(db.fetchById[ResidentAppearance](id).copy(reviewed = true))
    <html><body>
      Face label accepted.
      <a href="/faces/unreviewed">Return to label browser.</a>
    </body></html>
  }

  def relabel(id : Int, residentId : Int) : NodeSeq =
  {
    val updatedResident = {
      residentId match {
        case 0 => {
          None
        }
        case _ => {
          Some(db.fetchById[HomeResident](residentId))
        }
      }
    }
    db.save(db.fetchById[ResidentAppearance](id).copy(
      reviewed = true, resident = updatedResident))
    <html><body>
      Face relabeled.
      <a href="/faces/unreviewed">Return to label browser.</a>
    </body></html>
  }
}
