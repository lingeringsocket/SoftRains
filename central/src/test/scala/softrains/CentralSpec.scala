package softrains

import io.Source

import org.specs2.mutable._

class CentralSpec extends Specification
{
  // database state is shared, so we need isolation
  sequential

  private val central = new Central

  private def getDeviceCount =
    central.db.query[LanDevice].fetch.size

  private def getLanActiveCount =
    central.db.query[LanPresence].whereEqual("active", true).fetch.size

  private def getResidentCount =
    central.db.query[HomeResident].fetch.size

  private def getHomeActiveCount =
    central.db.query[HomePresence].whereEqual("active", true).fetch.size

  private def readResource(resource : String) =
    Source.fromFile(getClass.getResource(resource).getPath)

  "Central" should
  {
    "seed DB" in
    {
      getHomeActiveCount must be equalTo 0
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getResidentCount must be equalTo 2
    }

    "scan devices" in
    {
      getLanActiveCount must be equalTo 0
      getDeviceCount must be equalTo 2
      getHomeActiveCount must be equalTo 0
      central.scanLanSource(readResource("/devices1.html"))
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
      central.scanLanSource(readResource("/devices2.html"))
      getLanActiveCount must be equalTo 4
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 2
      central.scanLanSource(readResource("/devices1.html"))
      getLanActiveCount must be equalTo 3
      getDeviceCount must be equalTo 4
      getHomeActiveCount must be equalTo 1
    }
  }
}
