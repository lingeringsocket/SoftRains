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
import softrains.network._
import softrains.kiosk._
import softrains.vision._

import akka.actor._
import akka.event._

import scala.concurrent.ExecutionContext.Implicits.global

object CentralActor
{
  // request a notification scan
  case object ScanNotificationsMsg extends SoftRainsMsg
}

class CentralActor(central : CentralService) extends Actor
{
  import CentralActor._

  private val settings = SoftRainsActorSettings(context)

  private val networkScanInterval =
    settings.Router.scanInterval

  private val notificationScanInterval =
    settings.Residents.notificationScanInterval

  private val log = Logging(context.system, this)

  private val deviceMonitorActor = context.actorOf(
    Props(classOf[DeviceMonitorActor], central),
    "deviceMonitorActor")

  private val kioskSpec = settings.Actors.kiosk
  if (!kioskSpec.isEmpty) {
    context.actorOf(Props(classOf[KioskActor]), kioskSpec)
  }

  override def preStart()
  {
    if (networkScanInterval.length > 0) {
      context.system.scheduler.schedule(
        networkScanInterval, networkScanInterval,
        deviceMonitorActor, DeviceMonitorActor.ScanNetworkMsg)
    }
    context.system.scheduler.schedule(
      notificationScanInterval, notificationScanInterval,
      self, CentralActor.ScanNotificationsMsg)
  }

  def receive =
  {
    case ScanNotificationsMsg => {
      central.scanNotifications
    }
    case msg : CameraActor.FaceDetectedMsg => {
      val db = central.db
      val residentOpt = db.query[HomeResident].
        whereEqual("name", msg.name.capitalize).fetchOne
      residentOpt.foreach(resident => {
        db.save(ResidentAppearance(
          Some(resident), readClockTime, msg.faceFile, msg.sceneFile))
      })
    }
  }
}
