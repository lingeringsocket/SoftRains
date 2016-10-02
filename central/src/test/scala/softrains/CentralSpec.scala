package softrains

import com.typesafe.config._

import io.Source

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
  private val settings = CentralSettings(ConfigFactory.load)
  private val central = new CentralService(settings, deviceMonitor)

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
      central.scanLan(true)
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
      deviceMonitor.setHtml(readResource("/devices2.html"))
      central.scanLan(true)
      getLanActiveCount must be equalTo 4
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 2
      deviceMonitor.setHtml(readResource("/devices1.html"))
      central.scanLan(true)
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
    }

    "handle scan failure" in
    {
      getExceptionCount must be equalTo 0
      val startTime = central.readClockTime
      deviceMonitor.setHtml("<blah>")
      central.scanLan(true)
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