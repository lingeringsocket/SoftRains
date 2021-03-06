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
import softrains.intercom._

import akka.actor._

object ConversationActor
{
  // received messages
  final case class ActivateMsg(
    topic : ConversationTopic, channel : ActorRef,
    partner : ConversationPartner = ConversationPartner.ALLISON)
      extends SoftRainsMsg

  // sent messages
  // see IntercomActor received messages

  sealed trait State
  sealed trait Data

  case object Inactive extends State
  case object Pairing extends State
  case object Preempting extends State
  case object Paired extends State

  case object Empty extends Data
  final case class PairingData(
    topic : ConversationTopic, channel : ActorRef,
    partner : ConversationPartner)
      extends Data
  final case class ConvoData(
    topic : ConversationTopic,
    replies : Iterator[IntercomActor.SpeakerSoundMsg] = Iterator.empty)
      extends Data
}
import ConversationActor._
import CommunicationPriority._

class ConversationActor(
  db : CentralDb, ontology : CentralOntology) extends LoggingFSM[State, Data]
    with ConversationContext
{
  import IntercomActor._

  private val settings = SoftRainsActorSettings(context)

  private var currentTranscript
      : Option[ConversationTranscript with sorm.Persisted] = None

  private var lastUtterance
      : Option[ConversationUtterance] = None

  private var conversationContext : ConversationContext = this

  private var partner = ConversationPartner.ALLISON

  startWith(Inactive, Empty)

  onTransition {
    case Paired -> Inactive => {
      currentTranscript.foreach(transcript => {
        db.save(transcript.copy(endTime = Some(readClockTime)))
      })
      currentTranscript = None
      lastUtterance = None
      conversationContext = this
    }
  }

  when(Inactive) {
    case Event(ActivateMsg(topic, channel, newPartner), _) => {
      channel ! PairRequestMsg
      goto(Pairing) using PairingData(topic, channel, newPartner)
    }
  }

  when(Pairing) {
    case Event(BusyMsg, PairingData(topic, channel, _)) => {
      topic.getPriority match {
        case EMERGENCY => {
          channel ! PairPreemptMsg
          goto(Preempting)
        }
        // TODO retry for ASAP?
        case _ => {
          goto(Inactive) using Empty
        }
      }
    }
    case Event(PairAcceptedMsg, PairingData(topic, channel, newPartner)) => {
      startConversation(topic, channel, newPartner)
      goto(Paired) using ConvoData(topic)
    }
  }

  when(Preempting) {
    case Event(PairAcceptedMsg, PairingData(topic, channel, newPartner)) => {
      // TODO:  insert apology for interrupting
      startConversation(topic, channel, newPartner)
      goto(Paired) using ConvoData(topic)
    }
  }

  when(Paired) {
    case Event(SpeakerSoundFinishedMsg(fileOpt), ConvoData(topic, replies)) => {
      if (!fileOpt.isEmpty) {
        lastUtterance.foreach(utterance => {
          db.save(utterance.copy(audioFile = fileOpt))
        })
      }
      if (replies.hasNext) {
        val reply = replies.next
        sendReply(reply)
        stay
      } else {
        if (topic.isInProgress) {
          sender ! PartnerListenMsg(
            topic.getPersonName, topic.useVoiceIdentification)
          stay
        } else {
          sender ! UnpairMsg
          goto(Inactive) using Empty
        }
      }
    }
    case Event(SilenceMsg, _) => {
      // TODO:  retry last?
      sender ! UnpairMsg
      goto(Inactive) using Empty
    }
    case Event(PersonUtteranceMsg(alternatives, personName, audioFile),
      ConvoData(topic, _)) =>
    {
      // FIXME: pass real start time of utterance as part of
      // PersonUtteranceMsg, making sure clocks are synchronized
      val utterance = alternatives.head
      saveUtterance(personName, utterance, audioFile)
      topic.consumeUtterance(utterance, personName, conversationContext)
      topic.consumeAlternatives(alternatives)
      topic.produceMessage(conversationContext) match {
        case Some(SpeakerSoundSeqMsg(seq)) => {
          val replies = seq.iterator
          if (replies.hasNext) {
            sendReply(replies.next)
            stay using ConvoData(topic, replies)
          } else {
            sender ! UnpairMsg
            goto(Inactive) using Empty
          }
        }
        case Some(reply) => {
          sendReply(reply)
          stay
        }
        case _ => {
          sender ! UnpairMsg
          goto(Inactive) using Empty
        }
      }
    }
    case Event(PreemptionDisconnectMsg, _) => {
      // TODO:  resume later?
      goto(Inactive) using Empty
    }
  }

  override def getSettings = settings

  override def getActorSystem = context.system

  override def getOntology = ontology

  override def getDatabase = db

  override def getTranscript = currentTranscript

  override def getPartner() = partner

  override def setPartner(newPartner : ConversationPartner)
  {
    partner = newPartner
  }

  private def startConversation(
    topic : ConversationTopic, channel : ActorRef,
    newPartner : ConversationPartner)
  {
    setPartner(newPartner)
    val startTime = readClockTime
    val transcript = db.save(ConversationTranscript(startTime))
    currentTranscript = Some(transcript)
    conversationContext = new ConversationSubContext(this, true)
    topic.produceMessage(conversationContext).foreach({ msg =>
      msg match {
        case PartnerUtteranceMsg(utterance, voice) => {
          lastUtterance = Some(db.save(ConversationUtterance(
            transcript, startTime, "SoftRains", utterance)))
          if (voice.isEmpty) {
            channel ! PartnerUtteranceMsg(utterance, partner.voiceName)
          } else {
            channel ! msg
          }
        }
        case _ => {
          channel ! msg
        }
      }
    })
  }

  private def saveUtterance(
    speakerName : String, utterance : String,
    audioFile : Option[String] = None)
  {
    currentTranscript.foreach(transcript =>
      lastUtterance = Some(db.save(ConversationUtterance(
        transcript, readClockTime, speakerName, utterance, audioFile))))
  }

  private def sendReply(reply : SpeakerSoundMsg)
  {
    reply match {
      case PartnerUtteranceMsg(utterance, voice) => {
        saveUtterance("SoftRains", utterance)
        if (voice.isEmpty) {
          sender ! PartnerUtteranceMsg(utterance, partner.voiceName)
        } else {
          sender ! reply
        }
      }
      case _ => {
        sender ! reply
      }
    }
  }

  initialize()
}
