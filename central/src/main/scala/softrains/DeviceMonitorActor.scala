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

import akka.actor._
import akka.event._

object DeviceMonitorActor
{
  // request a network scan
  case object ScanNetworkMsg
}

class DeviceMonitorActor(central : CentralService) extends Actor
{
  import DeviceMonitorActor._

  private val settings = CentralActorSettings(context)

  private val log = Logging(context.system, this)

  private val tenHours = 600

  private var nRequests = 0

  def receive =
  {
    case ScanNetworkMsg => {
      nRequests += 1
      if ((nRequests % tenHours) == 0) {
        log.info("Refreshing session")
        central.getDeviceMonitor.requestLogin
      }
      log.info("Scanning network")
      central.scanLan
      log.info("Network scan complete")
    }
  }
}
