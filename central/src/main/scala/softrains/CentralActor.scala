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

import scala.concurrent.ExecutionContext.Implicits.global

trait RemoteReady
{
}

trait CentralMsg
{
}

trait PeripheralMsg extends CentralMsg with RemoteReady
{
}

object CentralActor
{
}

class CentralActor(central : CentralService) extends Actor
{
  private val settings = CentralActorSettings(context)

  private val networkScanFreq = settings.Router.scanFreq

  private val log = Logging(context.system, this)

  private val deviceMonitorActor = context.actorOf(
    Props(classOf[DeviceMonitorActor], central),
    "deviceMonitorActor")

  override def preStart()
  {
    context.system.scheduler.schedule(
      networkScanFreq, networkScanFreq,
      deviceMonitorActor, DeviceMonitorActor.ScanNetworkMsg)
  }

  def receive =
  {
    case "test" => log.info("received test")
  }
}
