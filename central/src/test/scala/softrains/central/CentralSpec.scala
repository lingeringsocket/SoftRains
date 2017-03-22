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

import org.joda.time._

trait DateTimeOrderingImplicit
{
  implicit object DateTimeOrdering extends Ordering[DateTime]
  {
    def compare(d1: DateTime, d2: DateTime) = d1.compareTo(d2)
  }
}

class MockCableRouterMonitor
    extends AbstractCableRouterMonitor
{
  private var deviceHtml = ""

  def setHtml(html : String)
  {
    deviceHtml = html
  }

  override protected def fetchDevices() = deviceHtml

  override def loginIfNeeded()
  {
  }

  override def requestLogin()
  {
  }
}

class CentralSpec extends AkkaActorSpecification with DateTimeOrderingImplicit
{
  // database state is shared, so we need isolation
  sequential

  protected val deviceMonitor = new MockCableRouterMonitor

  protected val central = new CentralService(settings, deviceMonitor)

  protected def getCameraCount =
    central.db.query[CameraFeed].fetch.size

  protected def getDeviceCount =
    central.db.query[LanDevice].fetch.size

  protected def getLanActiveCount =
    central.db.query[LanPresence].whereEqual("active", true).fetch.size

  protected def getResidentCount =
    central.db.query[HomeResident].fetch.size

  protected def getHomeActiveCount =
    central.db.query[HomePresence].whereEqual("active", true).fetch.size

  protected def getExceptionCount =
    central.db.query[ExceptionReport].fetch.size

  "Central" should
  {
    "seed DB" in new AkkaActorExample
    {
      central.setActorSystem(system)
      getCameraCount must be equalTo 1
      getHomeActiveCount must be equalTo 0
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getResidentCount must be equalTo 2
      getExceptionCount must be equalTo 0
    }

    "scan devices" in new AkkaActorExample
    {
      val startTime = readClockTime
      central.findNewDevices(startTime).size must be equalTo 0
      central.setActorSystem(system)
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getHomeActiveCount must be equalTo 0
      deviceMonitor.setHtml(readResource("/devices1.html"))
      central.scanLan
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
      val tallyTime = readClockTime
      central.findNewDevices(startTime).size must be equalTo 3
      deviceMonitor.setHtml(readResource("/devices2.html"))
      central.scanLan
      getLanActiveCount must be equalTo 4
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 2
      deviceMonitor.setHtml(readResource("/devices1.html"))
      central.scanLan
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
      central.findNewDevices(tallyTime).size must be equalTo 1
    }

    "handle scan failure" in new AkkaActorExample
    {
      central.setActorSystem(system)
      getExceptionCount must be equalTo 0
      val startTime = readClockTime
      deviceMonitor.setHtml("<blah>")
      central.scanLan
      val endTime = readClockTime
      getExceptionCount must be equalTo 1
      val report = central.db.query[ExceptionReport].fetchOne.get
      report.message must contain("Unexpected device HTML")
      report.tryTime must beBetween(startTime, endTime)
      report.catchTime must beBetween(startTime, endTime)
      report.catchTime must beGreaterThanOrEqualTo(report.tryTime)
    }
  }
}
