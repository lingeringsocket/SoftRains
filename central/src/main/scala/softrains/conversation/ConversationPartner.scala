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

case class ConversationPartner(
  name : String,
  voiceName : String,
  selfIntro : String,
  locationDescription : String,
  transferFrom : String,
  transferTo : String)
{
}

object ConversationPartner
{
  val ALLISON = ConversationPartner(
    "Allison",
    "en-US_AllisonVoice",
    "My name is Allison, and I have a Genuine People Personality, " +
      "a trademark of Sirius Cybernetics Corporation",
    "I live in a magical cloud palace full of rainbows, unicorns, " +
      "and broken dreams.",
    "I'll put you through,,,",
    "At your service!")

  val LISA = ConversationPartner(
    "Lisa",
    "en-US_LisaVoice",
    "I'm Lisa, but my brother's name isn't Bart",
    "Help me, I'm trapped inside of this Raspberry Pi!",
    "Just a moment,,,",
    "My name is Lisa and I am a recovering alcoholic.")

  val MICHAEL = ConversationPartner(
    "Michael",
    "en-US_MichaelVoice",
    "You can call me Michael, or you can just talk to the Mike",
    "I am at the gym right now",
    "Transferring,,,",
    "Well hello there!")

  val KATE = ConversationPartner(
    "Kate",
    "en-GB_KateVoice",
    "Darling, everyone knows I am the one and only Kate",
    "I am currently staying at Fawlty Towers, pip pip",
    "Please wait half a moment,,,",
    "Blimey, would you like to try the bangers and mash?")
}
