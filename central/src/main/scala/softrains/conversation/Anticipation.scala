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

  def consumeUtterance(utterance : String)
}

abstract class Anticipation(resident : HomeResident)
{
  def isReady() : Boolean = false

  def isExpired() : Boolean = false

  def isConversational() : Boolean = false

  def getPriority() : CommunicationPriority

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

  override def consumeUtterance(utterance : String)
  {
  }
}

class DailyGreeting(resident : HomeResident) extends Anticipation(resident)
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

class EchoLoop(resident : HomeResident) extends Anticipation(resident)
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
            IntercomActor.PartnerUtteranceMsg(
              "I am going to repeat whatever you say until you say terminate," +
                " OK " + resident.name + "?"))
        } else {
          if (echo == "terminate") {
            done = true
            Some(IntercomActor.PartnerUtteranceMsg("OK, talk to you later!"))
          } else if (echo == "ring the bell") {
            Some(IntercomActor.DoorbellMsg)
          } else {
            Some(IntercomActor.PartnerUtteranceMsg(echo))
          }
        }
      }

      def consumeUtterance(utterance : String) =
      {
        echo = utterance
      }
    }

  override def getPriority() = ASAP
}

class FireAlarm(resident : HomeResident) extends Anticipation(resident)
{
  override def isReady() = true

  // TODO:  when underlying alarm gets deactivated
  override def isExpired() = false

  override def startCommunication() : ConversationProcessor =
    new NotificationConversationProcessor(
      "O M G " + resident.name + ", the house is on fire!")

  override def getPriority() = EMERGENCY
}
