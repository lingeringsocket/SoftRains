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
import softrains.central._

import akka.actor._

import org.joda.time._

object PersonalPronoun extends Enumeration
{
  type PersonalPronoun = Value
  val SOMEONE, I, YOU, HE, SHE = Value
}
import PersonalPronoun.PersonalPronoun

trait ConversationContext
{
  def getActorSystem : ActorSystem

  def getSettings : SoftRainsSettings

  def getOntology : CentralOntology

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

  def createNotification(notification : PendingNotification)
  {
    getDatabase.save(notification)
    val openhab = new CentralOpenhab(getActorSystem, getSettings)
    openhab.updateResidentNotificationFlag(notification.resident, true)
    openhab.ensureSuccess
  }

  def getPersonalPronoun() : PersonalPronoun = PersonalPronoun.SOMEONE

  def setPersonalPronoun(pronoun : PersonalPronoun)
  {}

  def getPersonName() : String = ""

  def setPersonName(name : String)
  {}

  def getPartner() : ConversationPartner = ConversationPartner.ALLISON

  def setPartner(newPartner : ConversationPartner)
  {}
}

class ConversationSubContext(
  parent : ConversationContext, writeParent : Boolean = false)
    extends ConversationContext
{
  private var partner = parent.getPartner

  private var person = parent.getPersonName

  private var pronoun = parent.getPersonalPronoun

  private var currentTime = parent.getCurrentTime

  override def getActorSystem = parent.getActorSystem

  override def getSettings = parent.getSettings

  override def getOntology = parent.getOntology

  override def getDatabase = parent.getDatabase

  override def getCurrentTime = currentTime

  override def getTranscript = parent.getTranscript

  override def getPersonName() : String = person

  override def setPersonName(name : String)
  {
    if (writeParent) {
      parent.setPersonName(name)
    }
    person = name
  }

  override def getPersonalPronoun() = pronoun

  override def setPersonalPronoun(newPronoun : PersonalPronoun)
  {
    if (writeParent) {
      parent.setPersonalPronoun(newPronoun)
    }
    pronoun = newPronoun
  }

  override def getPartner() = partner

  override def setPartner(newPartner : ConversationPartner)
  {
    if (writeParent) {
      parent.setPartner(newPartner)
    }
    partner = newPartner
  }

  def setCurrentTime(newTime : DateTime)
  {
    currentTime = newTime
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

  override def getOntology = unsupported

  override def setPersonName(name : String)
  {
    if (!name.isEmpty) {
      unsupported
    }
  }

  override def setPersonalPronoun(newPronoun : PersonalPronoun)
  {
    if (newPronoun != PersonalPronoun.SOMEONE) {
      unsupported
    }
  }

  override def setPartner(newPartner : ConversationPartner)
  {
    if (newPartner != ConversationPartner.ALLISON) {
      unsupported
    }
  }
}
