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
import softrains.intercom._

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._

import akka.pattern.ask
import akka.util._

import scala.concurrent._
import scala.util._
import scala.sys.process._

import org.joda.time.DateTime

class CentralHttp(central : CentralService)
{
  private val db = central.db

  private val startTime = readClockTime

  private var lastGreet : DateTime = readClockTime

  private def getIntercomActor = central.getIntercomActor

  def createRoute() =
  {
    val textContent = ContentTypes.`text/html(UTF-8)`
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
                  central.createNotification(PendingNotification(
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
          HttpEntity(textContent, "<h1>Ding Dong!</h1>")
        })
      }
    } ~
    path("volume" / "up") {
      get {
        complete({
          getIntercomActor ! IntercomActor.VolumeUpMsg
          HttpEntity(textContent, "<h1>Volume Increased!</h1>")
        })
      }
    } ~
    path("volume" / "down") {
      get {
        complete({
          getIntercomActor ! IntercomActor.VolumeDownMsg
          HttpEntity(textContent, "<h1>Volume Decreased!</h1>")
        })
      }
    } ~
    path("loop" / Segment) { file =>
      get {
        complete({
          getIntercomActor ! IntercomActor.StartAudioFileMsg(
            file, true)
          HttpEntity(textContent, s"<h1>Now Looping $file</h1>")
        })
      }
    } ~
    path("play" / Segment) { file =>
      get {
        complete({
          getIntercomActor ! IntercomActor.PlayAudioFileMsg(file)
          HttpEntity(textContent, s"<h1>Now Playing $file</h1>")
        })
      }
    } ~
    path("silence") {
      get {
        complete({
          getIntercomActor ! IntercomActor.StopAudioFileMsg
          HttpEntity(textContent, "<h1>Silence is Golden</h1>")
        })
      }
    } ~
    path("uptime") {
      get {
        complete({
          val checkTime = readClockTime
          val interval = computeUptime(startTime, checkTime)
          HttpEntity(textContent, "uptime in " + interval)
        })
      }
    } ~
    path("intercom" / "ping") {
      get {
        complete({
          try {
            val intercomActor = getIntercomActor
            val intercomActorTimeout = central.intercomActorTimeout
            val uptimeFuture = intercomActor.ask(
              IntercomActor.UptimeRequestMsg)(Timeout(intercomActorTimeout))
            Await.ready(uptimeFuture, intercomActorTimeout)
            HttpEntity(textContent, "ON")
          } catch {
            case ex : Exception => {
              HttpEntity(textContent, "OFF")
            }
          }
        })
      }
    } ~
    path("intercom" / "softreboot") {
      get {
        complete({
          val intercomActor = getIntercomActor
          intercomActor !
            IntercomActor.SystemUtteranceMsg(
              "Restarting the intercom, please wait")
          intercomActor !
            IntercomActor.StartAudioFileMsg("reboot.mp3", true, true)
          intercomActor !
            IntercomActor.RebootMsg(true)
          HttpEntity(textContent, "<h1>Soft reboot requested</h1>")
        })
      }
    } ~
    path("intercom" / "hardreboot") {
      get {
        complete({
          val command = central.getSettings.Kiosk.restartCommand
          if (!command.isEmpty) {
            command.!!
          }
          HttpEntity(textContent, "<h1>Hard reboot requested</h1>")
        })
      }
    } ~
    path("greet") {
      get {
        complete({
          val now = readClockTime
          if (lastGreet.isBefore(now.minusSeconds(10))) {
            lastGreet = now
            central.startConversation
          }
          HttpEntity(textContent, "Salutations!")
        })
      }
    } ~
    path("professor") {
      get {
        complete({
          central.startProfessor
          HttpEntity(textContent, "Yo!")
        })
      }
    } ~
    path("identify") {
      get {
        complete({
          central.startIdentify
          HttpEntity(textContent, "Guess Who?")
        })
      }
    } ~
    path("faces" / "all") {
      get {
        complete({
          new CentralFaces(central).labelsPage(false)
        })
      }
    } ~
    path("faces" / "accept") {
      get {
        complete({
          new CentralFaces(central).acceptAll()
        })
      }
    } ~
    path("faces" / "unreviewed") {
      get {
        complete({
          new CentralFaces(central).labelsPage(true)
        })
      }
    } ~
    path("faces" / IntNumber) { id =>
      get {
        complete({
          new CentralFaces(central).detailPage(id)
        })
      }
    } ~
    path("faces" / IntNumber / "delete") { id =>
      get {
        complete({
          new CentralFaces(central).delete(id)
        })
      }
    } ~
    path("faces" / IntNumber / "accept") { id =>
      get {
        complete({
          new CentralFaces(central).accept(id)
        })
      }
    } ~
    path("faces" / IntNumber / "relabel" / IntNumber) { (id, residentId) =>
      get {
        complete({
          new CentralFaces(central).relabel(id, residentId)
        })
      }
    } ~
    path("conversation") {
      get {
        complete({
          central.startConversation
          HttpEntity(textContent, "Yakkety yak yak!")
        })
      }
    }
  }
}
