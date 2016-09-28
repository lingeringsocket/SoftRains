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
