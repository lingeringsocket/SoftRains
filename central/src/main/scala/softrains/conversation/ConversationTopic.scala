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

import softrains.base._
import softrains.central._
import softrains.intercom._

import CommunicationPriority._

abstract class ConversationTopic
{
  def isInProgress() : Boolean = false

  protected def delegateToProduceMessage(
    context : ConversationContext = NullConversationContext) =
  {
    produceMessage(context) match {
      case Some(IntercomActor.PartnerUtteranceMsg(utterance, voice)) =>
        Some(utterance)
      case _ =>
        None
    }
  }

  protected def delegateToProduceUtterance(
    context : ConversationContext = NullConversationContext) =
    produceUtterance(context).map(IntercomActor.PartnerUtteranceMsg(_))

  def produceUtterance(context : ConversationContext = NullConversationContext)
      : Option[String]

  def produceMessage(context : ConversationContext = NullConversationContext)
      : Option[IntercomActor.SpeakerSoundMsg] =
    delegateToProduceUtterance(context)

  def consumeUtterance(
    utterance : String, personName : String = "",
    context : ConversationContext = NullConversationContext)

  def getPersonName() : String = ""

  def useVoiceIdentification() : Boolean = false

  def getPriority() : CommunicationPriority

  def getChangedTopic() : Option[ConversationTopic] = None
}

trait ConversationTopicSource
{
  def proposeTopicForPerson(
    context : ConversationContext,
    personName : String) : Option[ConversationTopic]

  def isExhausted : Boolean
}

class SequentialTopicSource(topics : Seq[ConversationTopic])
    extends ConversationTopicSource
{
  private val iterator = topics.iterator

  override def proposeTopicForPerson(
    context : ConversationContext,
    personName : String) =
  {
    if (iterator.hasNext) {
      Some(iterator.next)
    } else {
      None
    }
  }

  override def isExhausted = iterator.isEmpty
}

class TopicDispatcher(
  topicSource : ConversationTopicSource,
  personName : String = "",
  intro : String = "")
    extends ConversationTopic
{
  private var done = false

  private var turned = false

  private var pending : Option[IntercomActor.SpeakerSoundMsg] = Some(
    IntercomActor.PartnerUtteranceMsg(
      if (personName.isEmpty) {
        "Who goes there?"
      } else {
        if (intro.isEmpty) {
          "Hello, " + personName + ".  How are you?"
        } else {
          intro
        }
      }
    )
  )

  private var currentPerson = personName

  private var subTopic : Option[ConversationTopic] = None

  override def getPriority() = ONLY_IF_NOT_BUSY

  override def isInProgress() = !done

  override def getPersonName() =
  {
    val name = subTopic.map(_.getPersonName).getOrElse("")
    if (name.isEmpty) {
      currentPerson
    } else {
      name
    }
  }

  override def useVoiceIdentification() =
    subTopic.map(_.useVoiceIdentification).getOrElse(false)

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  override def produceMessage(context : ConversationContext) =
  {
    if (pending.isEmpty) {
      subTopic match {
        case Some(topic) => {
          val messageOpt = topic.produceMessage(context)
          if (messageOpt.isEmpty) {
            changeTopic(context)
            if (done) {
              softLanding
            } else {
              subTopic.get.produceUtterance(context) match {
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
              changeTopic(context)
            }
            messageOpt
          }
        }
        case _ => None
      }
    } else {
      val message = pending
      pending = None
      if (subTopic.isEmpty && topicSource.isExhausted) {
        turnTheTables
      }
      message
    }
  }

  private def changeTopic(context : ConversationContext)
  {
    subTopic match {
      case Some(lastTopic) => {
        val newTopic = lastTopic.getChangedTopic
        if (!newTopic.isEmpty) {
          subTopic = newTopic
          return
        }
      }
      case _ =>
    }
    topicSource.proposeTopicForPerson(context, currentPerson) match {
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

  override def consumeUtterance(
    utterance : String, speakingPerson : String,
    context : ConversationContext) =
  {
    subTopic match {
      case Some(topic) => topic.consumeUtterance(
        utterance, speakingPerson, context)
      case _ => {
        if (speakingPerson.isEmpty && currentPerson.isEmpty) {
          pending = Some(IntercomActor.PartnerUtteranceMsg(
            "Sorry, I don't recognize your voice."))
          done = true
        } else {
          if (!speakingPerson.isEmpty) {
            currentPerson = speakingPerson
          }
          changeTopic(context)
          subTopic match {
            case Some(topic) => {
              topic.produceMessage(context) match {
                case Some(message) => {
                  pending = Some(message)
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

  override def produceUtterance(context : ConversationContext) =
  {
    if (over) {
      None
    } else {
      over = true
      Some(getNotification)
    }
  }

  override def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext)
  {
  }
}

class MessageTopic(
  notification : PendingNotification, msg : IntercomActor.SpeakerSoundMsg)
    extends ConversationTopic
{
  private var over = false

  override def getPriority() = ASAP

  override def produceMessage(context : ConversationContext) =
  {
    context.getDatabase.save(
      notification.copy(receiveTime = Some(readClockTime)))
    if (over) {
      None
    } else {
      over = true
      Some(msg)
    }
  }

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  override def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext)
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

class EchoLoop extends ConversationTopic
{
  private var done = false
  private var echo = ""

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  override def produceMessage(context : ConversationContext) =
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

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
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

  override def getPersonName() : String =
  {
    if (counter < residents.size) {
      residents(counter).name
    } else {
      ""
    }
  }

  override def useVoiceIdentification() : Boolean = true

  override def produceUtterance(context : ConversationContext) =
  {
    if (counter == 0) {
      Some(
        getPersonName +
          ", please say, the quick brown fox jumped over the lazy dog.")
    } else if (counter < residents.size) {
      Some(
        "Now, " + getPersonName +
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

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
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
