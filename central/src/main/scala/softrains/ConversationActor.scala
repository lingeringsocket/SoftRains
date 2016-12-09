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
package softrains

import akka.actor._

object ConversationActor
{
  // TODO: parameterize
  val voiceName = "SoftRains"

  // received messages
  final case class ActivateMsg(anticipation : Anticipation, channel : ActorRef)
      extends CentralMsg

  sealed trait State
  sealed trait Data

  case object Inactive extends State
  case object Pairing extends State
  case object Preempting extends State
  case object Paired extends State

  case object Empty extends Data
  final case class PairingData(anticipation : Anticipation, channel : ActorRef)
      extends Data
  final case class ConvoData(
    anticipation : Anticipation, processor : ConversationProcessor)
      extends Data
}
import ConversationActor._
import CommunicationPriority._

class ConversationActor extends LoggingFSM[State, Data]
{
  import LandlineActor._

  private val settings = CentralActorSettings(context)

  startWith(Inactive, Empty)

  when(Inactive) {
    case Event(ActivateMsg(anticipation, channel), _) => {
      if (!anticipation.isReady || anticipation.isExpired) {
        stay
      } else {
        channel ! PairRequestMsg(voiceName)
        goto(Pairing) using PairingData(anticipation, channel)
      }
    }
  }

  when(Pairing) {
    case Event(BusyMsg, PairingData(anticipation, channel)) => {
      anticipation.getPriority match {
        case EMERGENCY => {
          channel ! PairPreemptMsg(voiceName)
          goto(Preempting)
        }
        // TODO retry for ASAP?
        case _ => {
          goto(Inactive) using Empty
        }
      }
    }
    case Event(PairAcceptedMsg, PairingData(anticipation, channel)) => {
      val processor = startConversation(anticipation, channel)
      goto(Paired) using ConvoData(anticipation, processor)
    }
  }

  when(Preempting) {
    case Event(PairAcceptedMsg, PairingData(anticipation, channel)) => {
      // TODO:  insert apology for interrupting
      val processor = startConversation(anticipation, channel)
      goto(Paired) using ConvoData(anticipation, processor)
    }
  }

  when(Paired) {
    case Event(SpeakerSoundFinishedMsg, ConvoData(anticipation, _)) => {
      if (anticipation.isConversational) {
        sender ! PartnerListenMsg
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
    case Event(PersonUtteranceMsg(utterance), ConvoData(_, processor)) => {
      processor.consumeUtterance(utterance)
      processor.produceMessage match {
        case Some(reply) => {
          sender ! reply
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

  private def startConversation(
    anticipation : Anticipation, channel : ActorRef) =
  {
    val processor = anticipation.startCommunication
    processor.produceMessage.foreach(channel ! _)
    processor
  }

  initialize()
}
