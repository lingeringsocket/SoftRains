package softrains

import com.typesafe.config._

import org.joda.time._

import java.io._

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

  private def loginIfNeeded()
  {
    if (!cookies.isEmpty) {
      return
    }
    val request = (url(settings.Router.url) / "check.php") << Map(
      "username" -> settings.Router.user,
      "password" -> settings.Router.password)
    val result = Http(request)
    cookies = result().getHeader("Set-Cookie")
  }

  def requestLogin()
  {
    cookies = ""
  }

  def runLan()
  {
    val tenHours = 600
    var nRequests = 0
    var suppressEmail = true
    while (true) {
      loginIfNeeded
      scanLan(suppressEmail)
      logEvent(
        "Active device count: " +
          db.query[LanPresence].whereEqual("active", true).fetch.size)
      Thread.sleep(60000)
      nRequests += 1
      if ((nRequests % tenHours) == 0) {
        requestLogin
      }
      suppressEmail = false
    }
  }

  def logEvent(msg : String)
  {
    println(msg)
  }

  def scanLan(suppressEmail : Boolean)
  {
    val scanTime = readClockTime
    try {
      val devicesHtml = fetchDevices
      scanDevices(devicesHtml, scanTime, suppressEmail)
    } catch {
      case ex : Exception => {
        handleException(scanTime, ex)
      }
    }
  }

  def scanLanString(devicesHtml : String)
  {
    val scanTime = readClockTime
    try {
      scanDevices(devicesHtml, readClockTime, true)
    } catch {
      case ex : Exception => {
        handleException(scanTime, ex)
      }
    }
  }

  def readClockTime = new DateTime(DateTimeZone.UTC)

  private def handleException(tryTime : DateTime, ex : Exception)
  {
    if (!settings.Test.active) {
      ex.printStackTrace
    }
    val catchTime = readClockTime
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    ex.printStackTrace(pw)
    pw.close
    db.save(ExceptionReport(tryTime, catchTime, sw.toString))
    requestLogin
  }

  private def fetchDevices() =
  {
    val request = (url(settings.Router.url) / "connected_devices_computers.php").
      addHeader("Cookie", cookies)
    val result = Http(request OK as.String)
    result()
  }

  private def scanDevices(
    html : String, scanTime : DateTime, suppressEmail : Boolean)
  {
    val parser = new HTML5Parser
    val xml = parser.loadString(html)
    val validationDivs = (xml \\ "div").filter(
      div => (div \ "@class").text.trim == "cnt-device-main")
    if (validationDivs.isEmpty) {
      throw new RuntimeException("Unexpected device HTML")
    }
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
        macAddress, scanTime, suppressEmail)
    }
    markInactiveDevices(scanTime)
    markInactiveResidents(scanTime, suppressEmail)
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

  private def markInactiveResidents(scanTime : DateTime, suppressEmail : Boolean)
  {
    val presences = db.query[HomePresence].
      whereEqual("active", true).
      whereSmaller("endTime", scanTime).
      fetch
    for (presence <- presences) {
      val resident = presence.resident
      db.save(presence.copy(active = false))
      logEvent("Resident departed:  " + resident.name)
      if (!suppressEmail) {
        sendMail(
          resident,
          "See ya later, " + resident.name + "!",
          "Have a nice day :)")
      }
    }
  }

  def sendMail(resident : HomeResident, subject : String, body : String)
  {
    import javax.mail._
    import javax.mail.internet._
    import java.util.Properties

    val properties = new Properties
    properties.put("mail.smtp.auth", "true")
    properties.put("mail.smtp.host", "smtp.gmail.com")
    properties.put("mail.smtp.socketFactory.port", "465")
    properties.put(
      "mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory")
    properties.put("mail.smtp.port", "465")
    val authenticator = new Authenticator {
      override protected def getPasswordAuthentication =
      {
        new PasswordAuthentication(
          settings.Mail.user,
          settings.Mail.password)
      }
    }
    val session = Session.getDefaultInstance(properties, authenticator)
    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(settings.Mail.user))
    settings.Residents.emailMap.get(resident.name) match {
      case Some(recipient) => {
        message.setRecipients(
          Message.RecipientType.TO,
          recipient)
        message.setSubject(subject)
        message.setText(body)
        Transport.send(message)
      }
      case _ =>
    }
  }

  private def updatePresence(
    name : String,
    displayName : String,
    connectionType : String,
    ipAddress : String,
    macAddress : String,
    scanTime : DateTime,
    suppressEmail : Boolean)
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
            logEvent("Resident arrived:  " + owner.name)
            if (!suppressEmail) {
              sendMail(
                owner,
                "Welcome Home, " + owner.name + "!",
                "Glad to see you back :)")
            }
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
  central.runLan
}
