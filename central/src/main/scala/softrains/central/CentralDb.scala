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

import softrains.base._

import sorm._
import org.joda.time._

import java.io._

object CommunicationPriority extends Enumeration
{
  type CommunicationPriority = Value
  val ONLY_IF_NOT_BUSY, ASAP, EMERGENCY = Value
}
import CommunicationPriority._

trait CentralDbEntity extends RemoteReady
{
}

case class HomeResident(
  name : String
) extends CentralDbEntity

case class ResidentAppearance(
  resident : Option[HomeResident],
  imageTime : DateTime,
  faceFile : String,
  sceneFile : String,
  reviewed : Boolean = false
) extends CentralDbEntity
{
  def generateSceneUrl(settings : SoftRainsSettings) : String =
  {
    settings.Visitors.videoUrl + "/faces/" + sceneFile
  }

  def generateFaceUrl(settings : SoftRainsSettings) : String =
  {
    settings.Visitors.videoUrl + "/faces/" + faceFile
  }

  def generateFacePath(settings : SoftRainsSettings) : File =
  {
    new File(settings.Files.videoPath + "/faces/" + faceFile)
  }
}

case class LanDevice(
  name : String,
  displayName : String,
  owner : Option[HomeResident] = None
) extends CentralDbEntity

case class LanPresence(
  device : LanDevice,
  startTime : DateTime,
  endTime : DateTime,
  active : Boolean,
  connectionType : String,
  ipAddress : String,
  macAddress : String
) extends CentralDbEntity

case class HomePresence(
  resident : HomeResident,
  startTime : DateTime,
  endTime : DateTime,
  active : Boolean
) extends CentralDbEntity

case class CameraFeed(
  name : String,
  url : String
) extends CentralDbEntity

case class ExceptionReport(
  tryTime : DateTime,
  catchTime : DateTime,
  message : String
) extends CentralDbEntity

case class ConversationTranscript(
  startTime : DateTime,
  endTime : Option[DateTime] = None
) extends CentralDbEntity

case class ConversationUtterance(
  transcript : ConversationTranscript,
  startTime : DateTime,
  person : String,
  text : String,
  audioFile : Option[String] = None
) extends CentralDbEntity

case class PendingNotification(
  resident : HomeResident,
  message : String,
  audioFile : Option[String],
  priority : CommunicationPriority,
  creationTime : DateTime,
  pushTime : Option[DateTime] = None,
  expirationTime : Option[DateTime] = None,
  receiveTime : Option[DateTime] = None
) extends CentralDbEntity

class CentralDb(settings : SoftRainsSettings) extends Instance (
  entities = Set(
    Entity[ExceptionReport](),
    Entity[ConversationTranscript](),
    Entity[ConversationUtterance](),
    Entity[PendingNotification](),
    Entity[CameraFeed](unique = Set() + Seq("name")),
    Entity[HomeResident](unique = Set() + Seq("name")),
    Entity[ResidentAppearance](),
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
