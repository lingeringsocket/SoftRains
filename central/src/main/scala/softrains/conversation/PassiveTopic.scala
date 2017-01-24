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

import CommunicationPriority._

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
