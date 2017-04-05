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

import QueryAssumption._

class ButlerPersona(topic : PassiveTopic)
    extends CommonPersona(topic)
{
  private def unknownFaceUrl =
    "http://pre10.deviantart.net/c51a/th/pre/i/2010/223/1/f/" +
      "patrick_and_spongebob_by_zero_cool_23.jpg"

  override def getMatcher() : Seq[TopicMatcher] =
  {
    super.getMatcher ++ Seq[TopicMatcher](
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
        Seq("home", "here", "come home", "arrive", "arrived"),
        reportLocation(ASSUME_TRUE)),
      ContainsTopicMatcher.string(
        Seq("message", "record", "voicemail"),
        recordVoicemail),
      ContainsTopicMatcher.string(
        Seq("echo", "parrot"),
        startEcho),
      ContainsTopicMatcher.string(
        Seq("away", "go out", "leave"),
        reportLocation(ASSUME_FALSE)),
      ContainsTopicMatcher.string(
        Seq("face"),
        showFace),
      ContainsTopicMatcher.message(
        Seq("stop", "quiet", "silent", "silence"),
        IntercomActor.StopAudioFileMsg)
    )
  }

  private def recordVoicemail() =
  {
    val context = getContext
    context.getPersonalPronoun match {
      case PersonalPronoun.I => {
        val resident = loadResident(getResidentName)
        topic.changeTopic(
          new RecordingTopic(resident, resident))
        ""
      }
      case PersonalPronoun.YOU => {
        "That's very sweet, but why don't you just tell me directly?"
      }
      case PersonalPronoun.HE | PersonalPronoun.SHE => {
        val sender = loadResident(getResidentName)
        val recipient = loadResident(context.getPersonName)
        topic.changeTopic(
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
    topic.changeTopic(new EchoTopic)
    ""
  }

  private def showFace() =
  {
    val context = getContext
    var subject = ""
    var url = ""
    val resident = searchResident(getResidentName).get
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
}
