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

class SystemPersona(topic : PassiveTopic)
    extends CommonPersona(topic)
{
  override def getMatcher() : Seq[TopicMatcher] =
  {
    super.getMatcher ++ Seq[TopicMatcher](
      ContainsTopicMatcher.message(
        Seq("allison", "alley", "ally", "done"),
        changePartner(getContext, ConversationPartner.ALLISON)),
      ContainsTopicMatcher.message(
        Seq("reboot", "restart"),
        IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PartnerUtteranceMsg(
            "Restarting the intercom, please wait"),
          IntercomActor.StartAudioFileMsg("reboot.mp3", true, true),
          IntercomActor.RebootMsg(true)
        )),
        true)
    )
  }
}
