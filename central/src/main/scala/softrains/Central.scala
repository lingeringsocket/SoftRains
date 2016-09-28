package softrains

import com.typesafe.config._

import org.joda.time._

import io.Source
import xml._
import xml.parsing.NoBindingFactoryAdapter

import dispatch._, Defaults._

class HTML5Parser extends NoBindingFactoryAdapter
{
  override def loadXML(source : InputSource, _p: SAXParser) =
  {
    loadXML(source)
  }

  def loadXML(source : InputSource) =
  {
    import nu.validator.htmlparser.{sax,common}
    import sax.HtmlParser
    import common.XmlViolationPolicy

    val reader = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(this)
    reader.parse(source)
    rootElem
  }
}

class Central
{
  private var cookies = ""

  val settings = CentralSettings(ConfigFactory.load)

  val db = new CentralDb(settings)
  seedDb

  private def getTdText(tbl : NodeSeq) : Seq[String] =
    (tbl \\ "td").map(_.text.trim)

  private def getSpanText(nodeSeq : NodeSeq) : Seq[String] =
    (nodeSeq \\ "span").map(_.text.trim)

  private def getInputValue(inputs : NodeSeq, name : String) : String =
  {
    val input = inputs.filter(input =>
      (input \ "@name").text.trim == name)
    (input \ "@value").text.trim
  }

  def seedDb()
  {
    val deviceMap = settings.Residents.deviceMap
    if (!deviceMap.isEmpty) {
      val residentCount = db.query[HomeResident].fetch.size
      if (residentCount == 0) {
        deviceMap.foreach {
          case (residentName, deviceName) => {
            val resident = db.save(HomeResident(residentName))
            val device = db.save(
              LanDevice(deviceName, deviceName, Some(resident)))
          }
        }
      }
    }
  }

  def login()
  {
    val request = (url(settings.Router.url) / "check.php") << Map(
      "username" -> settings.Router.user,
      "password" -> settings.Router.password)
    val result = Http(request)
    cookies = result().getHeader("Set-Cookie")
  }

  def runLan()
  {
    while (true) {
      scanLan
      println(
        "ACTIVE = " +
          db.query[LanPresence].whereEqual("active", true).fetch.size)
      Thread.sleep(60000)
    }
  }

  def scanLan()
  {
    val devicesHtml = fetchDevices
    scanDevices(devicesHtml)
  }

  def scanLanSource(src : Source)
  {
    val devicesHtml = src.getLines.mkString("\n")
    scanDevices(devicesHtml)
  }

  private def fetchDevices() =
  {
    val request = (url(settings.Router.url) / "connected_devices_computers.php").
      addHeader("Cookie", cookies)
    val result = Http(request OK as.String)
    result()
  }

  private def scanDevices(html : String)
  {
    val scanTime = new DateTime(DateTimeZone.UTC)
    val parser = new HTML5Parser
    val xml = parser.loadString(html)
    val forms = (xml \\ "form").filter(
      form => getSpanText(form).contains("Host Name:"))
    for (form <- forms) {
      val spanText = getSpanText(form)
      val name = spanText(1)
      val connectionType = spanText(3)
      val inputs = form \\ "input"
      val ipAddress = getInputValue(inputs, "staticIPAddress")
      val macAddress = getInputValue(inputs, "mac_address")
      updatePresence(
        name + " " + macAddress, name, connectionType, ipAddress,
        macAddress, scanTime)
    }
    markInactiveDevices(scanTime)
    markInactiveResidents(scanTime)
  }

  private def markInactiveDevices(scanTime : DateTime)
  {
    val presences = db.query[LanPresence].
      whereEqual("active", true).
      whereSmaller("endTime", scanTime).
      fetch
    for (presence <- presences) {
      db.save(presence.copy(active = false))
    }
  }

  private def markInactiveResidents(scanTime : DateTime)
  {
    val presences = db.query[HomePresence].
      whereEqual("active", true).
      whereSmaller("endTime", scanTime).
      fetch
    for (presence <- presences) {
      db.save(presence.copy(active = false))
    }
  }

  private def updatePresence(
    name : String,
    displayName : String,
    connectionType : String,
    ipAddress : String,
    macAddress : String,
    scanTime : DateTime)
  {
    val deviceOpt = db.query[LanDevice].whereEqual("name", name).fetchOne
    val device = deviceOpt match {
      case Some(existingDevice) => existingDevice
      case _ => {
        db.save(LanDevice(name, displayName))
      }
    }
    val lanPresenceOpt = db.query[LanPresence].
      whereEqual("device.id", device.id).
      whereEqual("active", true).
      limit(1).
      fetchOne
    lanPresenceOpt match {
      case Some(presence) => {
        // hmm, what if one of the other attributes changed?
        db.save(presence.copy(endTime = scanTime))
      }
      case _ => {
        db.save(LanPresence(
          device, scanTime, scanTime, true,
          connectionType, ipAddress,
          macAddress))
      }
    }
    device.owner match {
      case Some(owner) => {
        val homePresenceOpt = db.query[HomePresence].
          whereEqual("resident", owner).
          whereEqual("active", true).
          limit(1).
          fetchOne
        homePresenceOpt match {
          case Some(presence) => {
            db.save(presence.copy(endTime = scanTime))
          }
          case _ => {
            db.save(HomePresence(
              owner, scanTime, scanTime, true))
          }
        }
      }
      case _ =>
    }
  }
}

object CentralApp extends App
{
  val central = new Central
  central.login
  central.runLan
}
