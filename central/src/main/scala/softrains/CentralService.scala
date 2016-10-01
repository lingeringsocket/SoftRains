package softrains

import com.typesafe.config._

import org.joda.time._

import java.io._

class CentralService(settings : CentralSettings, deviceMonitor : DeviceMonitor)
{
  val db = new CentralDb(settings)
  seedDb

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

  def runLan()
  {
    val tenHours = 600
    var nRequests = 0
    var suppressEmail = true
    while (true) {
      deviceMonitor.loginIfNeeded
      scanLan(suppressEmail)
      logEvent(
        "Active device count: " +
          db.query[LanPresence].whereEqual("active", true).fetch.size)
      Thread.sleep(60000)
      nRequests += 1
      if ((nRequests % tenHours) == 0) {
        deviceMonitor.requestLogin
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
      scanDevices(scanTime, suppressEmail)
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
    deviceMonitor.requestLogin
  }

  private def scanDevices(
    scanTime : DateTime, suppressEmail : Boolean)
  {
    deviceMonitor.scanDevices.foreach(device =>
      updatePresence(device, scanTime, suppressEmail))
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
    deviceState : DeviceState,
    scanTime : DateTime,
    suppressEmail : Boolean)
  {
    val deviceOpt = db.query[LanDevice].
      whereEqual("name", deviceState.name).fetchOne
    val device = deviceOpt match {
      case Some(existingDevice) => existingDevice
      case _ => {
        db.save(LanDevice(deviceState.name, deviceState.displayName))
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
          deviceState.connectionType, deviceState.ipAddress,
          deviceState.macAddress))
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
  val settings = CentralSettings(ConfigFactory.load)
  val central = new CentralService(settings, new CableRouterMonitor(settings))
  central.runLan
}
