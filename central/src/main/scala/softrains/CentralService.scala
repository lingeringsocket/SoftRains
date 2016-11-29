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

import org.joda.time._

import java.io._

import akka.actor._

import scala.concurrent._

class CentralService(settings : CentralSettings, deviceMonitor : DeviceMonitor)
{
  val db = new CentralDb(settings)
  seedDb

  def getDeviceMonitor = deviceMonitor

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
    val cameraMap = settings.Cameras.urlMap
    if (!cameraMap.isEmpty) {
      val cameraCount = db.query[CameraFeed].fetch.size
      if (cameraCount == 0) {
        cameraMap.foreach {
          case (cameraName, url) => {
            db.save(CameraFeed(cameraName, url))
          }
        }
      }
    }
  }

  def runCamera()
  {
    db.query[CameraFeed].fetchOne.foreach(feed => {
      val view = new CameraDesktopView(feed)
      val input = new CameraFeedInput(feed)
      val sentinel = new CameraSentinel(input, view, settings)
      sentinel.enableVisitorDetection(true)
      sentinel.enableMotionRecording
      sentinel.run
    })
  }

  def runAgent()
  {
    val config = ConfigFactory.load()
    val system = ActorSystem("SoftRains", config)
    val props = Props(classOf[CentralActor], this)
    system.actorOf(props, "centralActor")
    Await.result(system.whenTerminated, scala.concurrent.duration.Duration.Inf)
  }

  def logEvent(msg : String)
  {
    println(msg)
  }

  def scanLan()
  {
    val scanTime = readClockTime
    try {
      deviceMonitor.loginIfNeeded
      scanDevices(scanTime)
      logEvent(
        "Active device count: " +
          db.query[LanPresence].whereEqual("active", true).fetch.size)
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

  private def scanDevices(scanTime : DateTime)
  {
    deviceMonitor.scanDevices.foreach(device =>
      updatePresence(device, scanTime))
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
      val resident = presence.resident
      db.save(presence.copy(active = false))
      logEvent("Resident departed:  " + resident.name)
      updateOpenhab(resident, "OFF")
    }
  }

  private def updateOpenhab(resident : HomeResident, state : String)
  {
    import dispatch._, Defaults._

    if (settings.Test.active) {
      return
    }

    val request = (url(settings.Openhab.url) / "rest" / "items" /
      (resident.name.toLowerCase + "_phone_radio") / "state") .
      PUT.setContentType("text/plain", "UTF-8").
      setBody(state) <:< Map("Accept" -> "application/json")
    val result = Http(request OK as.String).either
    result() match {
      case Right(content) =>
      case Left(StatusCode(code)) => println(
        "Openhab update failed with status code " + code)
      case _ => println("Openhab update mystery")
    }
  }

  private def sendMail(resident : HomeResident, subject : String, body : String)
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
    scanTime : DateTime)
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
          fetchOne
        homePresenceOpt match {
          case Some(presence) => {
            db.save(presence.copy(endTime = scanTime))
          }
          case _ => {
            db.save(HomePresence(
              owner, scanTime, scanTime, true))
            logEvent("Resident arrived:  " + owner.name)
            updateOpenhab(owner, "ON")
          }
        }
      }
      case _ =>
    }
  }
}

object CentralSingleton
{
  val settings = CentralSettings(ConfigFactory.load)
  val service = new CentralService(settings, new CableRouterMonitor(settings))
}

object CentralApp extends App
{
  CentralSingleton.service.runAgent
}
