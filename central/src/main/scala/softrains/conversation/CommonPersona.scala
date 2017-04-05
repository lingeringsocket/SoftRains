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
import softrains.central._

import QueryAssumption._

class CommonPersona(topic : PassiveTopic)
{
  def getContext = topic.getContext

  def getResidentName = topic.getResidentName

  def getMatcher() : Seq[TopicMatcher] =
  {
    Seq[TopicMatcher](
      ContainsTopicMatcher.string(
        Seq("name", "who", "who", "who", "whose", "who's", "who're"),
        reportIdentity),
      ContainsTopicMatcher.string(
        Seq("where", "where's", "where", "where're"),
        reportLocation(ASSUME_NOTHING)),
      ContainsTopicMatcher.string(
        Seq("goodbye", "bye"),
        "Talk to you later!",
        true),
      ContainsTopicMatcher.string(
        Seq("thanks", "thank you"),
        "You are very welcome!")
    )
  }

  protected def changePartner(
    context : ConversationContext,
    newPartner : ConversationPartner) =
  {
    val oldPartner = context.getPartner
    context.setPartner(newPartner)
    IntercomActor.SpeakerSoundSeqMsg(Seq(
      IntercomActor.PartnerUtteranceMsg(
        oldPartner.transferFrom, oldPartner.voiceName),
      IntercomActor.PartnerUtteranceMsg(
        newPartner.transferTo, newPartner.voiceName)))
  }

  protected def searchResident(name : String) =
  {
    getContext.getDatabase.query[HomeResident].whereEqual("name", name).
      fetchOne
  }

  protected def loadResident(name : String) =
    searchResident(name).getOrElse(HomeResident("Stranger"))

  protected def clueless() = "I am not sure who or what you are referring to."

  protected def reportIdentity() =
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
          if (context.getPersonName == getResidentName) {
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

  protected def reportLocation(assumption : QueryAssumption) =
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
}
