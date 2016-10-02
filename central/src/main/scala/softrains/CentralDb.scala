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

import sorm._
import org.joda.time._

case class HomeResident(
  name : String)

case class LanDevice(
  name : String,
  displayName : String,
  owner : Option[HomeResident] = None)

case class LanPresence(
  device : LanDevice,
  startTime : DateTime,
  endTime : DateTime,
  active : Boolean,
  connectionType : String,
  ipAddress : String,
  macAddress : String)

case class HomePresence(
  resident : HomeResident,
  startTime : DateTime,
  endTime : DateTime,
  active : Boolean
)

case class ExceptionReport(
  tryTime : DateTime,
  catchTime : DateTime,
  message : String
)

class CentralDb(settings : CentralSettings) extends Instance (
  entities = Set(
    Entity[ExceptionReport](),
    Entity[HomeResident](unique = Set() + Seq("name")),
    Entity[HomePresence](),
    Entity[LanDevice](unique = Set() + Seq("name")),
    Entity[LanPresence]()),
  url = settings.Db.url,
  user = settings.Db.user,
  password = settings.Db.password,
  initMode = {
    if (settings.Test.active) {
      InitMode.DropAllCreate
    } else {
      InitMode.Create
    }
  }
)
