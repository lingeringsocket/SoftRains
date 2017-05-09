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

import softrains.intercom._

class ProfessorPersona(topic : PassiveTopic)
    extends CommonPersona(topic)
{
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
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, hope you find it calming"),
          IntercomActor.StartAudioFileMsg("vacuum.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("lullaby", "brahms"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, enjoy your nap!"),
          IntercomActor.StartAudioFileMsg("lullaby.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("korean"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, on young ha say yo!"),
          IntercomActor.StartAudioFileMsg("korean.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("vivaldi", "relax", "four seasons"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, let your stress melt away!"),
          IntercomActor.StartAudioFileMsg("vivaldi.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("shark", "pink fong"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, I love this one!"),
          IntercomActor.StartAudioFileMsg("pinkfong.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("play time", "playtime", "muffin man"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, everybody dance!"),
          IntercomActor.StartAudioFileMsg("playtime.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("folk", "kingston trio"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg("OK, groovy!"),
          IntercomActor.StartAudioFileMsg("folk.mp3", true)))),
      ContainsTopicMatcher.message(
        Seq("stop", "quiet", "silent", "silence"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.StopAudioFileMsg,
          IntercomActor.PartnerUtteranceMsg("OK, enjoy the silence")))),
      ContainsTopicMatcher.message(
        Seq("alexa", "amazon"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg(
            "Just a moment,,,"),
          IntercomActor.WakeAlexaMsg)))
    )
  }
}
