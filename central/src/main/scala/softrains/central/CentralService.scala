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
package softrains.central

import softrains.base._
import softrains.conversation._
import softrains.intercom._
import softrains.network._

import com.typesafe.config._

import org.joda.time._

import java.io._
import scala.io._
import scala.util._

import scala.concurrent._
import scala.collection.mutable._

import akka.actor._
import akka.pattern.ask
import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream._
import akka.util._

import org.joda.time.DateTime

class CentralService(
  settings : SoftRainsSettings, deviceMonitor : DeviceMonitor)
    extends ConversationContext
{
  private var actorSystem : Option[ActorSystem] = None

  val db = new CentralDb(settings)
  seedDb

  private var intercomActorLocal : Option[ActorRef] = None

  private var conversationActor : ActorRef = null

  private var lastGreet : DateTime = readClockTime

  def getDeviceMonitor = deviceMonitor

  def setActorSystem(system : ActorSystem)
  {
    actorSystem = Some(system)
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

  /*
  def runCamera()
  {
    db.query[CameraFeed].fetchOne.foreach(feed => {
      val view = new CameraDesktopView(feed.name)
      val input = new CameraFeedInput(feed.url)
      val sentinel = new CameraSentinel(input, view, settings)
      sentinel.enableFaceDetection(true)
      sentinel.enableVisitorDetection
      sentinel.enableMotionRecording
      sentinel.run
    })
  }
   */

  private def intercomActorTimeout =
    duration.FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS)

  private def getIntercomActor =
  {
    intercomActorLocal.getOrElse {
      val intercomSpec = settings.Actors.intercom
      assert(intercomSpec.startsWith("akka:"))
      val intercomActorSelection = getActorSystem.actorSelection(intercomSpec)
      val intercomActorFuture = intercomActorSelection.resolveOne(
        intercomActorTimeout)
      Await.result(
        intercomActorFuture, intercomActorTimeout)
    }
  }

  def runActors()
  {
    implicit val system = getActorSystem
    val centralSpec = settings.Actors.central
    if (!centralSpec.isEmpty) {
      val props = Props(classOf[CentralActor], this)
      system.actorOf(props, centralSpec)
    }
    val intercomSpec = settings.Actors.intercom
    if (!intercomSpec.isEmpty) {
      if (!intercomSpec.startsWith("akka:")) {
        val props = Props(classOf[IntercomActor])
        intercomActorLocal = Some(system.actorOf(props, intercomSpec))
      }
      val echoSpec = settings.Actors.echo
      val conversationProps = Props(classOf[ConversationActor], db)
      val conversationSpec = {
        if (echoSpec.isEmpty) {
          "conversationActor"
        } else {
          echoSpec
        }
      }
      conversationActor =
        system.actorOf(conversationProps, conversationSpec)
      if (!echoSpec.isEmpty) {
        startEcho
      }
    }

    val startTime = readClockTime
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val contentType = ContentTypes.`text/html(UTF-8)`
    val route = {
      pathPrefix("notification") {
        pathEndOrSingleSlash {
          post {
            formFields(
              'resident, 'message, 'priority, 'pushAfter, 'expireAfter)
            { (resident : String, message : String, priority : String,
              pushAfter : String, expireAfter : String) =>
              complete {
                val recipientOpt = db.query[HomeResident].
                  whereEqual("name", resident).fetchOne
                val priorityOpt = Try(CommunicationPriority.withName(priority))
                (recipientOpt, priorityOpt) match {
                  case (Some(recipient), Success(priority)) => {
                    val creationTime = readClockTime
                    val pushTime =
                      creationTime.plusMinutes(pushAfter.toInt)
                    val expirationTime =
                      creationTime.plusMinutes(expireAfter.toInt)
                    createNotification(PendingNotification(
                      recipient,
                      message,
                      None,
                      priority,
                      creationTime,
                      Some(pushTime),
                      Some(expirationTime),
                      None
                    ))
                    StatusCodes.OK
                  }
                  case _ => {
                    StatusCodes.NotFound
                  }
                }
              }
            }
          }
        }
      } ~
      path("doorbell") {
        get {
          complete({
            getIntercomActor ! IntercomActor.DoorbellMsg
            HttpEntity(contentType, "<h1>Ding Dong!</h1>")
          })
        }
      } ~
      path("loop" / Segment) { file =>
        get {
          complete({
            getIntercomActor ! IntercomActor.StartAudioFileMsg(
              file, true)
            HttpEntity(contentType, s"<h1>Now Looping $file</h1>")
          })
        }
      } ~
      path("play" / Segment) { file =>
        get {
          complete({
            getIntercomActor ! IntercomActor.StartAudioFileMsg(
              file, false)
            HttpEntity(contentType, s"<h1>Now Playing $file</h1>")
          })
        }
      } ~
      path("silence") {
        get {
          complete({
            getIntercomActor ! IntercomActor.StopAudioFileMsg
            HttpEntity(contentType, "<h1>Silence is Golden</h1>")
          })
        }
      } ~
      path("uptime") {
        get {
          complete({
            val checkTime = readClockTime
            val interval = computeUptime(startTime, checkTime)
            HttpEntity(contentType, "uptime in " + interval)
          })
        }
      } ~
      path("intercom" / "ping") {
        get {
          complete({
            try {
              val intercomActor = getIntercomActor
              val uptimeFuture = intercomActor.ask(
                IntercomActor.UptimeRequestMsg)(Timeout(intercomActorTimeout))
              Await.ready(uptimeFuture, intercomActorTimeout)
              HttpEntity(contentType, "ON")
            } catch {
              case ex : Exception => {
                HttpEntity(contentType, "OFF")
              }
            }
          })
        }
      } ~
      path("greet") {
        get {
          complete({
            val now = readClockTime
            if (lastGreet.isBefore(now.minusSeconds(10))) {
              lastGreet = now
              startConversation
            }
            HttpEntity(contentType, "Salutations!")
          })
        }
      } ~
      path("identify") {
        get {
          complete({
            startIdentify
            HttpEntity(contentType, "Guess Who?")
          })
        }
      } ~
      path("conversation") {
        get {
          complete({
            startConversation
            HttpEntity(contentType, "Yakkety yak yak!")
          })
        }
      } ~
      path("echo") {
        get {
          complete({
            startEcho
            HttpEntity(contentType, "Time to play!")
          })
        }
      }
    }

    val address = settings.Http.address
    val port = settings.Http.port
    val bindingFuture = Http().bindAndHandle(route, address, port)

    println(s"Akka started, HTTP listening at http://$address:$port/")
    println("Press RETURN to stop...")
    StdIn.readLine
    bindingFuture
      .flatMap(_.unbind)
      .onComplete(_ => system.terminate)
    Await.result(system.whenTerminated, duration.Duration.Inf)
  }

  private def startEcho()
  {
    val topic = new EchoLoop
    conversationActor ! ConversationActor.ActivateMsg(
      topic,
      getIntercomActor)
  }

  override def getSettings = settings

  override def getActorSystem = actorSystem.get

  override def getDatabase = db

  private def startConversation()
  {
    val openhabUrl = settings.Openhab.url
    def greet(name : String) = {
      val topicSource = new PersonalizedTopicSource
      val context = new ConversationSubContext(this)
      topicSource.preloadTopicsForPerson(context, name)
      // eagerly clear notification flag
      scanNotifications
      val intro = topicSource.generateGreeting(context)
      val dispatcher = new TopicDispatcher(topicSource, name, intro)
      conversationActor ! ConversationActor.ActivateMsg(
        dispatcher,
        getIntercomActor)
    }
    if (openhabUrl.isEmpty) {
      greet("Stranger")
    } else {
      val nameUrl = openhabUrl + "/rest/items/face_name/state"
      val httpConsumer = new HttpConsumer(getActorSystem)
      httpConsumer.fetchString(nameUrl) {
        name => {
          val residentName = name.capitalize
          val openhab = new CentralOpenhab(getActorSystem, settings)
          if (!openhab.getResidentPrivacy(HomeResident(residentName))) {
            greet(residentName)
          }
        }
      }
      httpConsumer.ensureSuccess
    }
  }

  private def startIdentify()
  {
    val residents = Seq(
      HomeResident("John"), HomeResident("Sujin"), HomeResident("Lara"))
    val topic = new VoiceIdentifier(residents)
    conversationActor ! ConversationActor.ActivateMsg(
      topic,
      getIntercomActor)
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
    val openhab = new CentralOpenhab(getActorSystem, settings)
    deviceMonitor.scanDevices.foreach(device =>
      updatePresence(openhab, device, scanTime))
    markInactiveDevices(scanTime)
    markInactiveResidents(openhab, scanTime)
    openhab.ensureSuccess
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

  private def markInactiveResidents(
    openhab : CentralOpenhab, scanTime : DateTime)
  {
    val presences = db.query[HomePresence].
      whereEqual("active", true).
      whereSmaller("endTime", scanTime).
      fetch
    for (presence <- presences) {
      val resident = presence.resident
      db.save(presence.copy(active = false))
      logEvent("Resident departed:  " + resident.name)
      openhab.updateResidentPhoneRadio(resident, "OFF")
    }
  }

  def findNewDevices(lastTallyTime : DateTime) =
  {
    db.fetchWithSql[LanDevice](
      "select id from lan_device where " +
        "(select min(start_time) from lan_presence " +
        "where device$id = lan_device.id) > ?",
      lastTallyTime)
  }

  def scanNotifications()
  {
    val now = readClockTime
    val openhab = new CentralOpenhab(getActorSystem, settings)

    // push out any ripe notification
    db.query[PendingNotification].
      whereEqual("receiveTime", None).
      whereLarger("expirationTime.item", now).
      fetch.foreach(notification => {
        notification.pushTime.foreach(time => {
          if (time.isBefore(now)) {
            openhab.sendResidentNotification(
              notification.resident,
              notification.message)
            db.save(notification.copy(
              pushTime = Some(now),
              receiveTime = Some(now)))
          }
        })
      })

    // turn off notifications flags when expired
    db.fetchWithSql[HomeResident](
      "select id from home_resident r where not exists(" +
        "select * from pending_notification " +
        "where receive_time is null " +
        "and resident$id = r.id " +
        "and ((expiration_time is null) or (expiration_time > ?)))",
      now).foreach(resident => {
        openhab.updateResidentNotificationFlag(resident, false)
      })
    openhab.ensureSuccess
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
    openhab : CentralOpenhab,
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
            openhab.updateResidentPhoneRadio(owner, "ON")
          }
        }
      }
      case _ =>
    }
  }
}

object CentralApp extends App
{
  val config = ConfigFactory.load
  val actorSystem = ActorSystem("SoftRains", config)
  val settings = SoftRainsSettings(config)
  val service = new CentralService(
    settings,
    new CableRouterMonitor(actorSystem, settings))
  service.setActorSystem(actorSystem)
  service.runActors
}
