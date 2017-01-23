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

import akka.actor._

import org.joda.time._

import CommunicationPriority._

trait ConversationContext
{
  private var person = ""

  def getActorSystem : ActorSystem

  def getSettings : SoftRainsSettings

  def getDatabase : CentralDb

  def getCurrentTime : DateTime = readClockTime

  def getTranscript :
      Option[ConversationTranscript with sorm.Persisted] = None

  def getUtterances() :
      Seq[ConversationUtterance] =
  {
    getTranscript match {
      case Some(transcript) => {
        getDatabase.query[ConversationUtterance].
          whereEqual("transcript.id", transcript.id).
          order("startTime").
          fetch
      }
      case _ => Seq.empty
    }
  }

  def getPerson() : String = person

  def setPerson(name : String)
  {
    person = name
  }
}

object NullConversationContext extends ConversationContext
{
  private def unsupported =
  {
    throw new UnsupportedOperationException("NullConversationContext")
  }

  override def getActorSystem = unsupported

  override def getSettings = unsupported

  override def getDatabase = unsupported
}

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
}

trait ConversationTopicSource
{
  def proposeTopicForPerson(
    context : ConversationContext,
    personName : String) : Option[ConversationTopic]
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
    if (response.isEmpty) {
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
      val message = IntercomActor.PartnerUtteranceMsg(response)
      response = ""
      Some(message)
    }
  }

  private def changeTopic(context : ConversationContext)
  {
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
    utterance : String, personName : String, context : ConversationContext) =
  {
    subTopic match {
      case Some(topic) => topic.consumeUtterance(
        utterance, personName, context)
      case _ => {
        if (personName.isEmpty && currentPerson.isEmpty) {
          response = "Sorry, I don't recognize your voice."
          done = true
        } else {
          if (!personName.isEmpty) {
            currentPerson = personName
          }
          changeTopic(context)
          subTopic match {
            case Some(topic) => {
              // FIXME what if they start out with message
              // instead of utterance?
              topic.produceUtterance(context) match {
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

class DailyGreeting(resident : HomeResident)
    extends NotificationTopic
{
  override protected def getNotification() =
    "Good morning, " + resident.name + "!"

  override def getPriority() = ASAP
}

class ContainsTopicMatcher(
  phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg,
  done : Boolean = false)
    extends TopicMatcher
{
  override def isDefinedAt(input : String) = phrases.exists(
    phrase => input.contains(phrase))

  override def apply(input : String) = (response, done)
}

object ContainsTopicMatcher
{
  def string(
    phrases : Seq[String], response : => String) =
  {
    new ContainsTopicMatcher(
      phrases, IntercomActor.PartnerUtteranceMsg(response), false)
  }

  def string(
    phrases : Seq[String], response : => String, done : Boolean) =
  {
    new ContainsTopicMatcher(
      phrases, IntercomActor.PartnerUtteranceMsg(response), done)
  }

  def message(
    phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg) =
  {
    new ContainsTopicMatcher(phrases, response, false)
  }

  def message(
    phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg,
    done : Boolean) =
  {
    new ContainsTopicMatcher(phrases, response, done)
  }
}

object EmptyTopicMatcher extends TopicMatcher
{
  override def isDefinedAt(input : String) = input.isEmpty

  override def apply(input : String) =
    (IntercomActor.PartnerUtteranceMsg("So, what is on your mind?"), false)
}

object EchoTopicMatcher extends TopicMatcher
{
  override def isDefinedAt(input : String) = true

  override def apply(input : String) =
    (IntercomActor.PartnerUtteranceMsg("I think you said, " + input), false)
}

class PassiveTopic(name : String) extends ConversationTopic
{
  private var done = false

  private var echo = ""

  private var contextOpt : Option[ConversationContext] = None

  private def getContext = contextOpt.getOrElse(NullConversationContext)

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  private val matcher = Seq[TopicMatcher](
    EmptyTopicMatcher,
    ContainsTopicMatcher.string(
      Seq("goodbye", "good bye"),
      "Talk to you later!",
      true),
    ContainsTopicMatcher.string(
      Seq("thanks", "thank you"),
      "You are very welcome!"),
    ContainsTopicMatcher.message(
      Seq("motor function"),
      IntercomActor.PlayAudioFileMsg("okay.mp3")),
    ContainsTopicMatcher.message(
      Seq("christmas"),
      IntercomActor.StartAudioFileMsg("JingleBells.mp3", true)),
    ContainsTopicMatcher.message(
      Seq("new year"),
      IntercomActor.StartAudioFileMsg("AuldLangSyne.mp3", false),
      true),
    ContainsTopicMatcher.message(
      Seq("poem", "story"),
      IntercomActor.StartAudioFileMsg("nicholas.wav", false),
      true),
    ContainsTopicMatcher.message(
      Seq("hodor", "hold the door"),
      IntercomActor.PlayAudioFileMsg("hodor.mp3")),
    ContainsTopicMatcher.message(
      Seq("ring the bell", "big ben", "ding dong", "knock knock",
        "anybody home"),
      IntercomActor.DoorbellMsg),
    ContainsTopicMatcher.message(
      Seq("michael", "mike"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "Transferring,,,"),
        IntercomActor.PartnerUtteranceMsg(
          "Well hello there!", "en-US_MichaelVoice")))),
    ContainsTopicMatcher.message(
      Seq("allison", "alley", "ally"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "I'll put you through,,,"),
        IntercomActor.PartnerUtteranceMsg(
          "At your service!", "en-US_AllisonVoice")))),
    ContainsTopicMatcher.message(
      Seq("lisa"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "Just a moment,,,"),
        IntercomActor.PartnerUtteranceMsg(
          "My name is Lisa and I am a recovering alcoholic.",
          "en-US_LisaVoice")))),
    ContainsTopicMatcher.message(
      Seq("kate", "england", "britain", "british", "english"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "Please wait half a second,,,"),
        IntercomActor.PartnerUtteranceMsg(
          "Blimey, would you like to try the bangers and mash?",
          "en-GB_KateVoice")))),
    ContainsTopicMatcher.message(
      Seq("alexa", "amazon"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "OK, fine,,,"),
        IntercomActor.WakeAlexaMsg))),
    ContainsTopicMatcher.message(
      Seq("play back", "play it back"),
      IntercomActor.SpeakerSoundSeqMsg(
        getContext.getUtterances().map(utterance =>
          IntercomActor.PlayAudioFileMsg(
            utterance.audioFile.getOrElse("hodor.mp3"))))),
    ContainsTopicMatcher.string(
      Seq("where is"),
      checkPresence),
    ContainsTopicMatcher.message(
      Seq("stop", "quiet", "silen"),
      IntercomActor.StopAudioFileMsg),
    EchoTopicMatcher
  ).reduce {
    (a : TopicMatcher, b : TopicMatcher) => a orElse b
  }

  private def checkPresence() =
  {
    val context = getContext
    if (context.getPerson.isEmpty) {
      "I am not sure who or what you are referring to."
    } else {
      val resident = new HomeResident(context.getPerson)
      val openhabPrivacy = new CentralOpenhab(
        context.getActorSystem, context.getSettings)
      if (openhabPrivacy.getResidentPrivacy(resident)) {
        "I'm sorry, I am not at liberty to answer that right now."
      } else {
        val openhabPresence = new CentralOpenhab(
          context.getActorSystem, context.getSettings)
        if (openhabPresence.getResidentPresence(resident)) {
          "I believe " + resident.name + " is currently at home."
        } else {
          "I believe " + resident.name + " is currently away from home."
        }
      }
    }
  }

  override def produceMessage(context : ConversationContext) =
  {
    contextOpt = Some(context)
    try {
      val response = matcher.lift(echo)
      if (response.isEmpty) {
        done = true
        None
      } else {
        val (msg, finished) = response.get
        if (finished) {
          done = true
        }
        Some(msg)
      }
    } finally {
      contextOpt = None
    }
  }

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
  {
    echo = utterance.toLowerCase
    if (echo.contains("sujin") || echo.contains("wife")) {
      context.setPerson("Sujin")
    } else if (echo.contains("john") || echo.contains("husband")) {
      context.setPerson("John")
    }
  }
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
