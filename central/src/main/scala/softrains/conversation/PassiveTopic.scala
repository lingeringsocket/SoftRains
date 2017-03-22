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
package softrains.conversation

import softrains.central._
import softrains.intercom._

import CommunicationPriority._
import QueryAssumption._

class PassiveTopic(residentName : String) extends ConversationTopic
{
  private def unknownFaceUrl =
    "http://pre10.deviantart.net/c51a/th/pre/i/2010/223/1/f/" +
      "patrick_and_spongebob_by_zero_cool_23.jpg"

  private var done = false

  private var lastUtterance = ""

  private var changedTopic : Option[ConversationTopic] = None

  private var contextOpt : Option[ConversationContext] = None

  private var catchAll = new CatchAllTopicMatcher

  private def getContext = contextOpt.getOrElse(NullConversationContext)

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  private val matcher = Seq[TopicMatcher](
    ContainsTopicMatcher.string(
      Seq("goodbye", "bye"),
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
      changePartner(getContext, ConversationPartner.MICHAEL)),
    ContainsTopicMatcher.message(
      Seq("allison", "alley", "ally"),
      changePartner(getContext, ConversationPartner.ALLISON)),
    ContainsTopicMatcher.message(
      Seq("lisa"),
      changePartner(getContext, ConversationPartner.LISA)),
    ContainsTopicMatcher.message(
      Seq("kate", "england", "britain", "british", "english"),
      changePartner(getContext, ConversationPartner.KATE)),
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
      Seq("where", "where's", "where", "where're"),
      reportLocation(ASSUME_NOTHING)),
    ContainsTopicMatcher.string(
      Seq("home", "here", "come home", "arrive", "arrived"),
      reportLocation(ASSUME_TRUE)),
    ContainsTopicMatcher.string(
      Seq("message", "record", "voicemail"),
      recordVoicemail),
    ContainsTopicMatcher.string(
      Seq("echo", "parrot"),
      startEcho),
    ContainsTopicMatcher.message(
      Seq("reboot", "restart"),
      IntercomActor.SpeakerSoundSeqMsg(Seq(
        IntercomActor.PartnerUtteranceMsg(
          "Restarting the intercom, please wait"),
        IntercomActor.StartAudioFileMsg("reboot.mp3", true, true),
        IntercomActor.RebootMsg(true)
      )),
      true),
    ContainsTopicMatcher.string(
      Seq("away", "go out", "leave"),
      reportLocation(ASSUME_FALSE)),
    ContainsTopicMatcher.string(
      Seq("name", "who", "who", "who", "whose", "who's", "who're"),
      reportIdentity),
    ContainsTopicMatcher.string(
      Seq("face"),
      showFace),
    ContainsTopicMatcher.message(
      Seq("stop", "quiet", "silent", "silence"),
      IntercomActor.StopAudioFileMsg),
    catchAll
  ).reduce {
    (a : TopicMatcher, b : TopicMatcher) => a orElse b
  }

  private def changePartner(
    context : ConversationContext,
    newPartner : ConversationPartner) =
  {
    val oldPartner = context.getPartner
    context.setPartner(newPartner)
    IntercomActor.SpeakerSoundSeqMsg(Seq(
      IntercomActor.PartnerUtteranceMsg(
        oldPartner.transferFrom),
      IntercomActor.PartnerUtteranceMsg(
        newPartner.transferTo, newPartner.voiceName)))
  }

  def clueless() = "I am not sure who or what you are referring to."

  private def recordVoicemail() =
  {
    val context = getContext
    context.getPersonalPronoun match {
      case PersonalPronoun.I => {
        val resident = loadResident(residentName)
        changedTopic = Some(
          new RecordingTopic(resident, resident))
        ""
      }
      case PersonalPronoun.YOU => {
        "That's very sweet, but why don't you just tell me directly?"
      }
      case PersonalPronoun.HE | PersonalPronoun.SHE => {
        val sender = loadResident(residentName)
        val recipient = loadResident(context.getPersonName)
        changedTopic = Some(
          new RecordingTopic(sender, recipient))
        ""
      }
      case _ => {
        clueless
      }
    }
  }

  private def startEcho() =
  {
    changedTopic = Some(new EchoTopic)
    ""
  }

  private def showFace() =
  {
    val context = getContext
    var subject = ""
    var url = ""
    val resident = searchResident(residentName).get
    context.getPersonalPronoun match {
      case PersonalPronoun.I => {
        subject = "Your Face"
        url = findFaceUrl(resident)
      }
      case PersonalPronoun.YOU => {
        subject = "My Face"
        url = "https://enderender.files.wordpress.com/2010/07/" +
          "vlcsnap-2010-07-25-21h58m13s121.png"
      }
      case _ => {
        subject = "Someone's Face"
        url = unknownFaceUrl
        if (!context.getPersonName.isEmpty) {
          searchResident(context.getPersonName) match {
            case Some(thirdPerson) => {
              subject = context.getPersonName + "'s Face"
              url = findFaceUrl(thirdPerson)
            }
            case _ =>
          }
        }
      }
    }
    CentralMail.sendMail(context.getSettings, resident, subject, url)
    "OK, I have sent an image link to your email."
  }

  private def findFaceUrl(resident : HomeResident with sorm.Persisted) =
  {
    getContext.getDatabase.query[ResidentAppearance].
      whereEqual("resident.id", resident.id).
      order("imageTime", true).
      fetchOne.
      map(_.generateSceneUrl(getContext.getSettings)).
      getOrElse(unknownFaceUrl)
  }

  private def reportIdentity() =
  {
    val context = getContext
    context.getPersonalPronoun match {
      case PersonalPronoun.I => {
        if (context.getPersonName.isEmpty) {
          clueless
        } else {
          "I am fairly sure you are " + context.getPersonName
        }
      }
      case PersonalPronoun.YOU => {
        context.getPartner.selfIntro
      }
      case _ => {
        if (context.getPersonName.isEmpty) {
          clueless
        } else {
          if (context.getPersonName == residentName) {
            context.getPersonName +
              ", are you suffering from an identity crisis?"
          } else {
            context.getPersonName +
              " is my favorite human, other than yourself"
          }
        }
      }
    }
  }

  private def searchResident(name : String) =
  {
    getContext.getDatabase.query[HomeResident].whereEqual("name", name).
      fetchOne
  }

  private def loadResident(name : String) =
    searchResident(name).getOrElse(HomeResident("Stranger"))

  private def reportLocation(assumption : QueryAssumption) =
  {
    val context = getContext
    context.getPersonalPronoun match {
      case PersonalPronoun.I => {
        "Silly human, you are standing right in front of me!"
      }
      case PersonalPronoun.YOU => {
        context.getPartner.locationDescription
      }
      case _ => {
        if (context.getPersonName.isEmpty) {
          clueless
        } else {
          val resident = loadResident(context.getPersonName)
          val openhabPrivacy = new CentralOpenhab(
            context.getActorSystem, context.getSettings)
          if (openhabPrivacy.getResidentPrivacy(resident)) {
            "I'm sorry, I am not at liberty to answer that right now."
          } else {
            val openhabPresence = new CentralOpenhab(
              context.getActorSystem, context.getSettings)
            val present = openhabPresence.getResidentPresence(resident)
            val confirmation =
              QueryAssumption.generateConfirmation(assumption, present)
            confirmation + {
              if (present) {
                "I believe " + resident.name + " is currently at home."
              } else {
                "I believe " + resident.name + " is currently away from home."
              }
            }
          }
        }
      }
    }
  }

  override def produceMessage(context : ConversationContext) =
  {
    contextOpt = Some(context)
    try {
      val response = matcher.lift(lastUtterance)
      catchAll.clearFirst
      if (response.isEmpty) {
        done = true
        None
      } else {
        val (msg, finished) = response.get
        if (finished) {
          done = true
        }
        if (!changedTopic.isEmpty) {
          done = true
          None
        } else {
          Some(msg)
        }
      }
    } finally {
      contextOpt = None
    }
  }

  override def getChangedTopic() = changedTopic

  private def forgetPerson(context : ConversationContext)
  {
    context.setPersonalPronoun(PersonalPronoun.SOMEONE)
    context.setPersonName("")
  }

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
  {
    lastUtterance = utterance.toLowerCase
    val inputSplit = ContainsTopicMatcher.splitWords(lastUtterance)
    if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("sujin", "lee")))
    {
      context.setPersonalPronoun(PersonalPronoun.SHE)
      context.setPersonName("Sujin")
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("my wife")))
    {
      if (residentName == "John") {
        context.setPersonalPronoun(PersonalPronoun.SHE)
        context.setPersonName("Sujin")
      } else {
        forgetPerson(context)
      }
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("john", "sichi", "see key")))
    {
      context.setPersonalPronoun(PersonalPronoun.HE)
      context.setPersonName("John")
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("my husband")))
    {
      if (residentName == "Sujin") {
        context.setPersonalPronoun(PersonalPronoun.HE)
        context.setPersonName("John")
      } else {
        forgetPerson(context)
      }
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("he", "his", "him")))
    {
      if (context.getPersonalPronoun != PersonalPronoun.HE) {
        forgetPerson(context)
      }
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("she", "her", "hers")))
    {
      if (context.getPersonalPronoun != PersonalPronoun.SHE) {
        forgetPerson(context)
      }
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("me", "i", "my", "myself")))
    {
      context.setPersonalPronoun(PersonalPronoun.I)
      context.setPersonName(residentName)
    } else if (ContainsTopicMatcher.matchPhrases(
      inputSplit, Seq("you", "your", "you're")))
    {
      context.setPersonalPronoun(PersonalPronoun.YOU)
      context.setPersonName("")
    } else {
      forgetPerson(context)
    }
  }
}
