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

import com.typesafe.config._

import scala.io.Source

import org.specs2.mutable._

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

class CentralSpec extends Specification with DateTimeOrderingImplicit
{
  // database state is shared, so we need isolation
  sequential

  private val deviceMonitor = new MockCableRouterMonitor
  private val settings = CentralSettings(ConfigFactory.load("test.conf"))
  private val central = new CentralService(settings, deviceMonitor)

  private def getCameraCount =
    central.db.query[CameraFeed].fetch.size

  private def getDeviceCount =
    central.db.query[LanDevice].fetch.size

  private def getLanActiveCount =
    central.db.query[LanPresence].whereEqual("active", true).fetch.size

  private def getResidentCount =
    central.db.query[HomeResident].fetch.size

  private def getHomeActiveCount =
    central.db.query[HomePresence].whereEqual("active", true).fetch.size

  private def readResource(resource : String) =
    Source.fromFile(getClass.getResource(resource).getPath).
      getLines.mkString("\n")

  private def getExceptionCount =
    central.db.query[ExceptionReport].fetch.size

  "Central" should
  {
    "seed DB" in
    {
      getCameraCount must be equalTo 1
      getHomeActiveCount must be equalTo 0
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getResidentCount must be equalTo 2
      getExceptionCount must be equalTo 0
    }

    "scan devices" in
    {
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getHomeActiveCount must be equalTo 0
      deviceMonitor.setHtml(readResource("/devices1.html"))
      central.scanLan
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
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
    }

    "handle scan failure" in
    {
      getExceptionCount must be equalTo 0
      val startTime = central.readClockTime
      deviceMonitor.setHtml("<blah>")
      central.scanLan
      val endTime = central.readClockTime
      getExceptionCount must be equalTo 1
      val report = central.db.query[ExceptionReport].fetchOne.get
      report.message must contain("Unexpected device HTML")
      report.tryTime must beBetween(startTime, endTime)
      report.catchTime must beBetween(startTime, endTime)
      report.catchTime must beGreaterThanOrEqualTo(report.tryTime)
    }
  }
}
