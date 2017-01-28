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
package softrains.conversation

import softrains.central._
import softrains.intercom._

import scala.collection.mutable._

import org.joda.time._

class PersonalizedTopicSource extends ConversationTopicSource
{
  private var currentPerson = ""

  private var iterator : Iterator[ConversationTopic] = Iterator.empty

  def preloadTopicsForPerson(
    context : ConversationContext, personName : String) =
  {
    if (personName != currentPerson) {
      currentPerson = personName
      val topics = new ArrayBuffer[ConversationTopic]
      val openhab = new CentralOpenhab(
        context.getActorSystem,
        context.getSettings)
      openhab.checkDoor("front_door", "front door")
      openhab.checkDoor("rear_door", "rear door")
      openhab.checkDoor("garage_door", "garage door")
      val results = openhab.retrieveResults
      if (!results.isEmpty) {
        topics += new WarningTopic(results)
      }
      val now = context.getCurrentTime
      val db = context.getDatabase
      db.fetchWithSql[PendingNotification](
        "select n.id from pending_notification n, home_resident r " +
          "where r.id=n.resident$id " +
          "and r.name=? " +
          "and receive_time is null " +
          "and (expiration_time is null or expiration_time > ?)",
        personName, now
      ).foreach(notification => {
        topics += new MessageTopic(
          notification.audioFile match {
            case Some(file) => {
              IntercomActor.SpeakerSoundSeqMsg(Seq(
                IntercomActor.PartnerUtteranceMsg(notification.message),
                IntercomActor.PlayAudioFileMsg(file)))
            }
            case _ => {
              IntercomActor.PartnerUtteranceMsg(notification.message)
            }
          })
        db.save(notification.copy(receiveTime = Some(now)))
      })
      iterator = topics.iterator
    }
  }

  override def proposeTopicForPerson(
    context : ConversationContext, personName : String) =
  {
    preloadTopicsForPerson(context, personName)
    if (iterator.hasNext) {
      Some(iterator.next)
    } else {
      None
    }
  }

  override def isExhausted = iterator.isEmpty

  private def toDayStart(dateTime : DateTime) =
  {
    dateTime.toDateTime(DateTimeZone.getDefault).withTimeAtStartOfDay
  }

  def generateGreeting(
    context : ConversationContext) : String =
  {
    val name = {
      if (currentPerson.isEmpty) {
        "Stranger"
      } else {
        currentPerson
      }
    }
    val currentTime = context.getCurrentTime
    val localTime = currentTime.toDateTime(DateTimeZone.getDefault)
    var omitExtended = false
    val utteranceOpt =
      context.getDatabase.query[ConversationUtterance].
        whereEqual("person", name).
        order("startTime", true).fetchOne
    val (recent, spokeToday) = utteranceOpt match {
      case Some(utterance) => {
        (utterance.startTime.isAfter(currentTime.minusMinutes(30)),
          toDayStart(utterance.startTime.minusHours(5)).equals(
            toDayStart(currentTime)))
      }
      case _ => (false, false)
    }
    val shortGreeting = {
      if (recent) {
        omitExtended = true
        "Hello again, " + name + "!"
      } else {
        val hour = localTime.hourOfDay.get
        if (hour < 3) {
          omitExtended = true
          name + ", shouldn't you be in bed?"
        } else if (hour < 12) {
          "Good morning, " + name + "!"
        } else if (hour < 18) {
          "Good afternoon, " + name + "!"
        } else {
          "Good evening, " + name + "!"
        }
      }
    }
    def dayOfWeek = localTime.dayOfWeek
    def extendedGreeting = dayOfWeek.get match {
      case DateTimeConstants.MONDAY => "Ready for another week?"
      case DateTimeConstants.WEDNESDAY => "Today is Hump Day!"
      case DateTimeConstants.FRIDAY => "Thank God it's Friday!"
      case DateTimeConstants.SUNDAY => "Did you meditate today?"
      case _ => "Happy " + dayOfWeek.getAsText + "!"
    }
    def statusOpt = {
      if (isExhausted) {
        if (omitExtended || !spokeToday) {
          ""
        } else {
          " How is your day going?"
        }
      } else {
        " I have some updates for you."
      }
    }
    if (spokeToday || omitExtended) {
      shortGreeting + statusOpt
    } else {
      shortGreeting + " " + extendedGreeting + statusOpt
    }
  }
}
