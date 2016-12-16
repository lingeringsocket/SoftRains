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

import org.joda.time._

object CommunicationPriority extends Enumeration
{
  type CommunicationPriority = Value
  val ONLY_IF_NOT_BUSY, ASAP, EMERGENCY = Value

}
import CommunicationPriority._

trait ConversationProcessor
{
  def produceUtterance() : Option[String]

  def produceMessage() : Option[IntercomActor.SpeakerSoundMsg] =
  {
    produceUtterance.map(IntercomActor.PartnerUtteranceMsg(_))
  }

  def consumeUtterance(utterance : String, personName : String)
}

abstract class ConversationTopic
{
  def isReady() : Boolean = false

  def isExpired() : Boolean = false

  def isConversational() : Boolean = false

  def getPriority() : CommunicationPriority

  def getNewSpeakerName() : String = ""

  def startCommunication() : ConversationProcessor
}

class NotificationConversationProcessor(notification : String)
    extends ConversationProcessor
{
  private var over = false

  override def produceUtterance() =
  {
    if (over) {
      None
    } else {
      over = true
      Some(notification)
    }
  }

  override def consumeUtterance(utterance : String, personName : String)
  {
  }
}

class DailyGreeting(resident : HomeResident)
    extends ConversationTopic
{
  private val expiration = (new DateTime).withTimeAtStartOfDay.plusDays(1)

  // TODO:  check whether resident is awake?
  override def isReady() = true

  // TODO:  expire if we already had some other conversation today
  override def isExpired() =
    DateTime.now.compareTo(expiration) > 0

  override def startCommunication() : ConversationProcessor =
    new NotificationConversationProcessor(
      "Good morning, " + resident.name + "!")

  override def getPriority() = ASAP
}

class GenericGreeting
    extends ConversationTopic
{
  override def isReady() = true

  override def isExpired() = false

  override def startCommunication() : ConversationProcessor =
  {
    val time = DateTime.now
    val hour = time.hourOfDay.get
    var includeDay = true
    val greetingTime = {
      if (hour < 3) {
        includeDay = false
        "Shouldn't you be in bed?"
      } else if (hour < 12) {
        "Good morning!"
      } else if (hour < 18) {
        "Good afternoon!"
      } else {
        "Good evening!"
      }
    }
    val fullGreeting = {
      if (includeDay) {
        val dayOfWeek = time.dayOfWeek
        val greetingDay = dayOfWeek.get match {
          case DateTimeConstants.MONDAY => "Ready for another week?"
          case DateTimeConstants.WEDNESDAY => "Today is Hump Day!"
          case DateTimeConstants.FRIDAY => "Thank God it's Friday!"
          case DateTimeConstants.SUNDAY => "Today is a good day for meditation."
          case _ => "Happy " + dayOfWeek.getAsText + "!"
        }
        greetingTime + " " + greetingDay
      } else {
        greetingTime
      }
    }
    new NotificationConversationProcessor(fullGreeting)
  }

  override def getPriority() = ASAP
}

class EchoLoop extends ConversationTopic
{
  private var done = false

  override def isReady() = true

  override def isExpired() = false

  override def isConversational() : Boolean = !done

  override def startCommunication() : ConversationProcessor =
    new ConversationProcessor {
      var echo = ""

      override def produceUtterance() = None

      override def produceMessage() =
      {
        if (echo.isEmpty) {
          Some(
            IntercomActor.PartnerUtteranceMsg("Polly wants a cracker!"))
        } else {
          if (echo == "terminate") {
            done = true
            Some(IntercomActor.PartnerUtteranceMsg("OK, talk to you later!"))
          } else if ((echo == "ring the bell") || (echo == "big ben")) {
            Some(IntercomActor.DoorbellMsg)
          } else {
            Some(IntercomActor.PartnerUtteranceMsg(echo))
          }
        }
      }

      def consumeUtterance(utterance : String, personName : String) =
      {
        echo = utterance.toLowerCase
      }
    }

  override def getPriority() = ASAP
}

class VoiceIdentifier(residents : Seq[HomeResident])
    extends ConversationTopic
{
  private var counter = 0

  override def isReady() = true

  override def isExpired() = false

  override def isConversational() : Boolean = true

  override def getNewSpeakerName() : String =
  {
    if (counter < residents.size) {
      residents(counter).name
    } else {
      ""
    }
  }

  private var lastUtterance = ""

  private var lastPerson = ""

  override def startCommunication() : ConversationProcessor =
    new ConversationProcessor {
      override def produceUtterance() =
      {
        if (counter == 0) {
          Some(
            residents.head.name +
              ", please say, the quick brown fox jumped over the lazy dog.")
        } else if (counter == 1) {
          Some(
            "Now, " + residents(counter).name +
              ", you say the same sentence.")
        } else if (counter == residents.size) {
          Some(
            "Now someone say anything and I will try to identify the speaker.")
        } else {
          Some(
            "I heard " + lastPerson + " say, " + lastUtterance +
              ".  Try another?")
        }
      }

      def consumeUtterance(utterance : String, personName : String) =
      {
        lastUtterance = utterance
        lastPerson = personName
        counter += 1
      }
    }

  override def getPriority() = ASAP
}

class FireAlarm(resident : HomeResident) extends ConversationTopic
{
  override def isReady() = true

  // TODO:  when underlying alarm gets deactivated
  override def isExpired() = false

  override def startCommunication() : ConversationProcessor =
    new NotificationConversationProcessor(
      "O M G " + resident.name + ", the house is on fire!")

  override def getPriority() = EMERGENCY
}
