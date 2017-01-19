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

import CommunicationPriority._

abstract class ConversationTopic
{
  def isInProgress() : Boolean = false

  protected def delegateToProduceMessage() =
  {
    produceMessage() match {
      case Some(IntercomActor.PartnerUtteranceMsg(utterance, voice)) =>
        Some(utterance)
      case _ =>
        None
    }
  }

  protected def delegateToProduceUtterance() =
    produceUtterance.map(IntercomActor.PartnerUtteranceMsg(_))

  def produceUtterance() : Option[String]

  def produceMessage() : Option[IntercomActor.SpeakerSoundMsg] =
    delegateToProduceUtterance

  def consumeUtterance(utterance : String, personName : String)

  def getNewSpeakerName() : String = ""

  def getPriority() : CommunicationPriority
}

trait ConversationTopicSource
{
  def proposeTopicForPerson(personName : String) : Option[ConversationTopic]
}

class SequentialTopicSource(topics : Seq[ConversationTopic])
    extends ConversationTopicSource
{
  private val iterator = topics.iterator

  override def proposeTopicForPerson(personName : String) =
  {
    if (iterator.hasNext) {
      Some(iterator.next)
    } else {
      None
    }
  }
}

class TopicDispatcher(
  topicSource : ConversationTopicSource,
  personName : String = "",
  intro : String = "")
    extends ConversationTopic
{
  private var done = false

  private var turned = false

  private var response = {
    if (personName.isEmpty) {
      "Who goes there?"
    } else {
      if (intro.isEmpty) {
        "Hello, " + personName + ".  How are you?"
      } else {
        intro
      }
    }
  }

  private var currentPerson = personName

  private var subTopic : Option[ConversationTopic] = None

  override def getPriority() = ONLY_IF_NOT_BUSY

  override def isInProgress() = !done

  override def getNewSpeakerName() =
    subTopic.map(_.getNewSpeakerName).getOrElse("")

  override def produceUtterance() = delegateToProduceMessage

  override def produceMessage() =
  {
    if (response.isEmpty) {
      subTopic match {
        case Some(topic) => {
          val messageOpt = topic.produceMessage
          if (messageOpt.isEmpty) {
            changeTopic
            if (done) {
              softLanding
            } else {
              subTopic.get.produceUtterance match {
                case Some(utterance) => {
                  Some(IntercomActor.PartnerUtteranceMsg(utterance))
                }
                case _ => {
                  softLanding
                }
              }

            }
          } else {
            if (!topic.isInProgress) {
              changeTopic
            }
            messageOpt
          }
        }
        case _ => None
      }
    } else {
      val message = IntercomActor.PartnerUtteranceMsg(response)
      response = ""
      Some(message)
    }
  }

  private def changeTopic()
  {
    topicSource.proposeTopicForPerson(currentPerson) match {
      case Some(newTopic) => {
        subTopic = Some(newTopic)
      }
      case _ => {
        turnTheTables
      }
    }
  }

  private def turnTheTables()
  {
    if (turned) {
      subTopic = None
      done = true
    } else {
      turned = true
      subTopic = Some(new PassiveTopic(currentPerson))
    }
  }

  private def softLanding() =
  {
    Some(IntercomActor.PartnerUtteranceMsg(
      "Well, " + currentPerson + ", it has been nice chatting with you!"))
  }

  override def consumeUtterance(utterance : String, personName : String) =
  {
    subTopic match {
      case Some(topic) => topic.consumeUtterance(
        utterance, personName)
      case _ => {
        if (personName.isEmpty) {
          response = "Sorry, I don't recognize your voice."
          done = true
        } else {
          currentPerson = personName
          changeTopic
          subTopic match {
            case Some(topic) => {
              // FIXME what if they start out with message
              // instead of utterance?
              topic.produceUtterance match {
                case Some(utterance) => {
                  response = {
                    utterance
                  }
                }
                case _ => {
                  turnTheTables
                }
              }
            }
            case _ => {
              turnTheTables
            }
          }
        }
      }
    }
  }
}

abstract class NotificationTopic
    extends ConversationTopic
{
  private var over = false

  protected def getNotification() : String

  override def produceUtterance() =
  {
    if (over) {
      None
    } else {
      over = true
      Some(getNotification)
    }
  }

  override def consumeUtterance(utterance : String, personName : String)
  {
  }
}

class DailyGreeting(resident : HomeResident)
    extends NotificationTopic
{
  override protected def getNotification() =
    "Good morning, " + resident.name + "!"

  override def getPriority() = ASAP
}

class GenericGreeting
    extends NotificationTopic
{
  override def getPriority() = ASAP

  override protected def getNotification() =
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
    fullGreeting
  }
}

class PassiveTopic(name : String) extends ConversationTopic
{
  private var done = false
  private var echo = ""

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance() = delegateToProduceMessage

  override def produceMessage() =
  {
    if (echo.isEmpty) {
      Some(
        IntercomActor.PartnerUtteranceMsg(
          "So, " + name + ", what is on your mind?"))
    } else {
      if ((echo.contains("goodbye")) || (echo.contains("good bye"))) {
        done = true
        Some(IntercomActor.PartnerUtteranceMsg("Talk to you later!"))
      } else if (echo.contains("thank you")) {
        Some(IntercomActor.PartnerUtteranceMsg("You are very welcome!"))
      } else if (echo.contains("motor function")) {
        Some(IntercomActor.PartnerUtteranceMsg("Okay."))
      } else if (echo.contains("christmas")) {
        done = true
        Some(IntercomActor.StartAudioFileMsg("JingleBells.mp3", true))
      } else if (echo.contains("new year")) {
        done = true
        Some(IntercomActor.StartAudioFileMsg("AuldLangSyne.mp3", true))
      } else if (echo.contains("story")) {
        done = true
        Some(IntercomActor.StartAudioFileMsg("nicholas.wav", false))
      } else if (echo.contains("hodor") || echo.contains("hold the door")) {
        Some(IntercomActor.StartAudioFileMsg("hodor.mp3", false))
      } else if ((echo == "ring the bell") || (echo == "big ben")) {
        Some(IntercomActor.DoorbellMsg)
      } else if (echo.contains("michael")) {
        Some(IntercomActor.PartnerUtteranceMsg(
          "Well hello there!", "en-US_MichaelVoice"))
      } else if (echo.contains("allison")) {
        Some(IntercomActor.PartnerUtteranceMsg(
          "At your service!", "en-US_AllisonVoice"))
      } else if (echo.contains("lisa")) {
        Some(IntercomActor.PartnerUtteranceMsg(
          "My name is Lisa and I am a recovering alcoholic.",
          "en-US_LisaVoice"))
      } else if (echo.contains("kate")) {
        Some(IntercomActor.PartnerUtteranceMsg(
          "Blimey, would you like to try the bangers and mash?",
          "en-GB_KateVoice"))
      } else if (echo.contains("alexa")) {
        Some(IntercomActor.WakeAlexaMsg)
      } else if (echo.contains("stop") || echo.contains("quiet") ||
        echo.contains("silen"))
      {
        Some(IntercomActor.StopAudioFileMsg)
      } else {
        Some(IntercomActor.PartnerUtteranceMsg("I think you said, " + echo))
      }
    }
  }

  def consumeUtterance(utterance : String, personName : String) =
  {
    echo = utterance.toLowerCase
  }
}

class EchoLoop extends ConversationTopic
{
  private var done = false
  private var echo = ""

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance() = delegateToProduceMessage

  override def produceMessage() =
  {
    if (echo.isEmpty) {
      Some(
        IntercomActor.PartnerUtteranceMsg("Polly wants a cracker!"))
    } else {
      if (echo == "terminate") {
        done = true
        None
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

class VoiceIdentifier(residents : Seq[HomeResident])
    extends ConversationTopic
{
  private var counter = 0

  private var done = false

  private var lastUtterance = ""

  private var lastPerson = ""

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def getNewSpeakerName() : String =
  {
    if (counter < residents.size) {
      residents(counter).name
    } else {
      ""
    }
  }

  override def produceUtterance() =
  {
    if (counter == 0) {
      Some(
        getNewSpeakerName +
          ", please say, the quick brown fox jumped over the lazy dog.")
    } else if (counter < residents.size) {
      Some(
        "Now, " + getNewSpeakerName +
          ", you say the same sentence.")
    } else if (counter == residents.size) {
      Some(
        "Now someone say anything and I will try to identify the speaker.")
    } else {
      if (lastUtterance == "stop") {
        done = true
        Some("Yes, right away!")
      } else {
        Some(
          "I heard " + lastPerson + " say, " + lastUtterance +
            ".  Try another?")
      }
    }
  }

  def consumeUtterance(utterance : String, personName : String) =
  {
    lastUtterance = utterance
    lastPerson = personName
    counter += 1
  }
}

class WarningTopic(warning : String) extends NotificationTopic
{
  override def getPriority() = ASAP

  override protected def getNotification() = warning
}
