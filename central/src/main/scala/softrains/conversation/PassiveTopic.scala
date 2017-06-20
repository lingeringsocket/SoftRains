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

import CommunicationPriority._

class PassiveTopic(residentName : String) extends ConversationTopic
{
  private var done = false

  private var lastUtterance = ""

  private var changedTopic : Option[ConversationTopic] = None

  private var contextOpt : Option[ConversationContext] = None

  private var catchAll = new CatchAllTopicMatcher

  def getContext = contextOpt.getOrElse(NullConversationContext)

  def getResidentName = residentName

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  private def constructMatcher = (constructMatcherForPersona ++
    Seq[TopicMatcher](catchAll)
  ).reduce {
    (a : TopicMatcher, b : TopicMatcher) => a orElse b
  }

  private def constructMatcherForPersona =
  {
    val persona = getContext.getPartner match {
      case ConversationPartner.ALLISON | ConversationPartner.KATE => {
        new ButlerPersona(this)
      }
      case ConversationPartner.LISA => {
        new SystemPersona(this)
      }
      case ConversationPartner.MICHAEL => {
        new ProfessorPersona(this)
      }
      case _ => {
        new CommonPersona(this)
      }
    }
    persona.getMatcher
  }

  private def processUtterance(utterance : String) =
  {
    constructMatcher.lift(utterance)
  }

  override def produceMessage(context : ConversationContext) =
  {
    contextOpt = Some(context)
    try {
      val response = processUtterance(lastUtterance)
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

  private[conversation] def changeTopic(newTopic : ConversationTopic)
  {
    changedTopic = Some(newTopic)
  }

  override def getChangedTopic() = changedTopic

  private def forgetPerson(context : ConversationContext)
  {
    context.setPersonalPronoun(PersonalPronoun.SOMEONE)
    context.setPersonName("")
  }

  override def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext)
  {
    lastUtterance = utterance.toLowerCase
    val inputSplit = ContainsTopicMatcher.splitWords(lastUtterance)
    val settings = context.getSettings
    val pronounMap = settings.Residents.pronounMap
    val aliasMap = settings.Residents.aliasMap
    val referenceMap = settings.Residents.referenceMap

    aliasMap.foreach({
      case (aliasedName, aliasList) => {
        if (ContainsTopicMatcher.matchPhrases(
          inputSplit, aliasList.split(",").toSeq))
        {
          context.setPersonalPronoun(
            pronounMap.get(aliasedName).map(
              PersonalPronoun.withName(_)).
              getOrElse(PersonalPronoun.HE))
          context.setPersonName(aliasedName)
          return
        }
      }
    })

    referenceMap.foreach({
      case (reference, aliasList) => {
        if (ContainsTopicMatcher.matchPhrases(
          inputSplit, aliasList.split(",").toSeq))
        {
          val refPair = reference.split(" calls ")
          val referencer = refPair.head
          val referenced = refPair.last
          if (referencer == residentName) {
            context.setPersonalPronoun(
              pronounMap.get(referenced).map(
                PersonalPronoun.withName(_)).
                getOrElse(PersonalPronoun.HE))
            context.setPersonName(referenced)
          } else {
            forgetPerson(context)
          }
          return
        }
      }
    })

    if (ContainsTopicMatcher.matchPhrases(
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
