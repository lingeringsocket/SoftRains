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

package softrains.snippet

import softrains._

class MainSnippet
{
  val central = CentralSingleton.service

  def renderResidents =
  {
    <lift:children>
    {
      val residents = central.db.query[HomeResident].fetch
      residents.map(resident => {
        val presenceOpt = central.db.query[HomePresence].
          whereEqual("resident", resident).
          whereEqual("active", true).
          fetchOne
        val locationSuffix = presenceOpt match {
          case Some(presence) => "-inside"
          case _ => "-outside"
        }
        <img id={resident.name} class={resident.name + locationSuffix}/>
      })
    }
    </lift:children>
  }

  def renderCameras =
  {
    <lift:children>
    {
      val cameras = central.db.query[CameraFeed].fetch
      cameras.map(camera => {
        <a id={camera.name} href={camera.url + "/browserfs.html"}>
          <img class="cctv"/>
        </a>
      })
    }
    </lift:children>
  }
}
