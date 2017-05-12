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

import softrains.base._
import softrains.intercom._

class ProfessorPersona(topic : PassiveTopic)
    extends CommonPersona(topic)
{
  private val stateUrl = getContext.getSettings.Openhab.url +
    "/rest/items/baby_sound/state"

  private def selectSoundTrack(utterance : String, soundtrack : String) =
  {
    val httpConsumer = new HttpConsumer(getContext.getActorSystem)
    httpConsumer.putString(stateUrl, soundtrack) {}
    httpConsumer.ensureSuccess
    IntercomActor.PartnerUtteranceMsg(utterance)
  }

  override def getMatcher() : Seq[TopicMatcher] =
  {
    super.getMatcher ++ Seq[TopicMatcher](
      ContainsTopicMatcher.message(
        Seq("allison"),
        changePartner(getContext, ConversationPartner.ALLISON)),
      ContainsTopicMatcher.message(
        Seq("system"),
        changePartner(getContext, ConversationPartner.LISA)),
      ContainsTopicMatcher.message(
        Seq("white noise"),
        selectSoundTrack(
          "OK. Hope you find it calming",
          "WHITE_NOISE"),
        true),
      ContainsTopicMatcher.message(
        Seq("lullaby", "brahms"),
        selectSoundTrack("OK. Enjoy your nap!",
          "LULLABY"),
        true),
      ContainsTopicMatcher.message(
        Seq("korean"),
        selectSoundTrack(
          "OK. On young ha say yo!",
          "KOREAN"),
        true),
      ContainsTopicMatcher.message(
        Seq("vivaldi", "relax", "four seasons"),
        selectSoundTrack(
          "OK. Let your stress melt away!",
          "RELAX"),
        true),
      ContainsTopicMatcher.message(
        Seq("shark", "pink fong"),
        selectSoundTrack(
          "OK. I love this one!",
          "PINKFONG"),
        true),
      ContainsTopicMatcher.message(
        Seq("play time", "playtime", "muffin man"),
        selectSoundTrack(
          "OK. Everybody dance!",
          "PLAYTIME"),
        true),
      ContainsTopicMatcher.message(
        Seq("folk", "kingston trio"),
        selectSoundTrack(
          "OK. Groovy!",
          "FOLK"),
        true),
      ContainsTopicMatcher.message(
        Seq("stop", "quiet", "silent", "silence"),
        selectSoundTrack(
          "OK. Enjoy the sound of silence.",
          "OFF"),
        true),
      ContainsTopicMatcher.message(
        Seq("alexa", "amazon"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg(
            "Just a moment,,,"),
          IntercomActor.WakeAlexaMsg)))
    )
  }
}
