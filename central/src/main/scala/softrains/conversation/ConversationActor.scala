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

import softrains.base._
import softrains.central._
import softrains.intercom._

import akka.actor._

object ConversationActor
{
  // received messages
  final case class ActivateMsg(
    topic : ConversationTopic, channel : ActorRef)
      extends SoftRainsMsg

  sealed trait State
  sealed trait Data

  case object Inactive extends State
  case object Pairing extends State
  case object Preempting extends State
  case object Paired extends State

  case object Empty extends Data
  final case class PairingData(
    topic : ConversationTopic, channel : ActorRef)
      extends Data
  final case class ConvoData(
    topic : ConversationTopic)
      extends Data
}
import ConversationActor._
import CommunicationPriority._

class ConversationActor(db : CentralDb) extends LoggingFSM[State, Data]
{
  import IntercomActor._

  private val settings = SoftRainsActorSettings(context)

  private var currentTranscript : Option[ConversationTranscript] = None

  startWith(Inactive, Empty)

  onTransition {
    case Paired -> Inactive => {
      currentTranscript.foreach(transcript => {
        db.save(transcript.copy(endTime = Some(readClockTime)))
      })
      currentTranscript = None
    }
  }

  when(Inactive) {
    case Event(ActivateMsg(topic, channel), _) => {
      channel ! PairRequestMsg
      goto(Pairing) using PairingData(topic, channel)
    }
  }

  when(Pairing) {
    case Event(BusyMsg, PairingData(topic, channel)) => {
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
    case Event(PairAcceptedMsg, PairingData(topic, channel)) => {
      startConversation(topic, channel)
      goto(Paired) using ConvoData(topic)
    }
  }

  when(Preempting) {
    case Event(PairAcceptedMsg, PairingData(topic, channel)) => {
      // TODO:  insert apology for interrupting
      startConversation(topic, channel)
      goto(Paired) using ConvoData(topic)
    }
  }

  when(Paired) {
    case Event(SpeakerSoundFinishedMsg, ConvoData(topic)) => {
      if (topic.isInProgress) {
        sender ! PartnerListenMsg(
          topic.getPersonName, topic.useVoiceIdentification)
        stay
      } else {
        sender ! UnpairMsg
        goto(Inactive) using Empty
      }
    }
    case Event(SilenceMsg, _) => {
      // TODO:  retry last?
      sender ! UnpairMsg
      goto(Inactive) using Empty
    }
    case Event(PersonUtteranceMsg(utterance, personName),
      ConvoData(topic)) =>
    {
      // FIXME: pass real start time of utterance as part of
      // PersonUtteranceMsg, making sure clocks are synchronized
      currentTranscript.foreach(transcript =>
        db.save(ConversationUtterance(
          transcript, readClockTime, personName, utterance)))
      topic.consumeUtterance(utterance, personName)
      topic.produceMessage match {
        case Some(reply) => {
          // FIXME:  use correct voice name
          currentTranscript.foreach(transcript =>
            reply match {
              case PartnerUtteranceMsg(utterance, _) => {
                db.save(ConversationUtterance(
                  transcript, readClockTime, "SoftRains", utterance))
              }
              case _ =>
            }
          )
          sender ! reply
          if (topic.isInProgress) {
            reply match {
              case StartAudioFileMsg(_,_) | StopAudioFileMsg => {
                sender ! PartnerListenMsg(
                  topic.getPersonName, topic.useVoiceIdentification)
              }
              case _ =>
            }
            stay
          } else {
            sender ! UnpairMsg
            goto(Inactive) using Empty
          }
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

  private def startConversation(
    topic : ConversationTopic, channel : ActorRef)
  {
    val startTime = readClockTime
    val transcript = db.save(ConversationTranscript(startTime))
    currentTranscript = Some(transcript)
    topic.produceMessage.foreach({ msg =>
      msg match {
        case PartnerUtteranceMsg(utterance, _) => {
          db.save(ConversationUtterance(
            transcript, startTime, "SoftRains", utterance))
        }
        case _ =>
      }
      channel ! msg
    })
  }

  initialize()
}
