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
package softrains.intercom

import softrains.base._

import akka.actor._

import scala.sys.process._

import org.joda.time.Seconds

object IntercomActor
{
  val PROTOCOL_ALREADY_PAIRED = "already paired"

  val PROTOCOL_UNPAIR_WITHOUT_PAIR = "cannot unpair without pairing first"

  val PROTOCOL_UTTERANCE_WITHOUT_PAIR = "must be paired before utterance"

  val PROTOCOL_LISTEN_WITHOUT_PAIR = "must be paired before listening"

  val PROTOCOL_RING_WHILE_PAIRED = "cannot ring phone while paired"

  val VOICE_NONE = "none"

  sealed trait State
  sealed trait Data

  // received messages
  trait SpeakerSoundMsg extends PeripheralMsg
  final case class PairRequestMsg(voice : String)
      extends PeripheralMsg
  final case class PairPreemptMsg(voice : String)
      extends PeripheralMsg
  final case class PartnerUtteranceMsg(utterance : String)
      extends SpeakerSoundMsg
  final case class PartnerListenMsg(newPersonName : String = "")
      extends PeripheralMsg
  case object UnpairMsg
      extends PeripheralMsg
  case object UptimeRequestMsg
      extends PeripheralMsg
  case object RingtoneMsg
      extends SpeakerSoundMsg
  case object DoorbellMsg
      extends SpeakerSoundMsg
  final case class StartAudioFileMsg(audioFile : String, loop : Boolean)
      extends SpeakerSoundMsg
  case object StopAudioFileMsg
      extends SpeakerSoundMsg

  // sent messages (to partner)
  case object BusyMsg
      extends PeripheralMsg
  final case class ProtocolErrorMsg(error : String)
      extends PeripheralMsg
  case object PairAcceptedMsg
      extends PeripheralMsg
  case object PreemptionDisconnectMsg
      extends PeripheralMsg
  final case class UptimeResponseMsg(seconds : Int)
      extends PeripheralMsg

  // sent messages (to parent)
  case object UnpairedMsg
      extends PeripheralMsg
  case object ListeningStartedMsg
      extends PeripheralMsg
  case object ListeningDoneMsg
      extends PeripheralMsg

  // forwarded messages (from watson to partner)
  trait WatsonCompletionMsg extends PeripheralMsg
  trait WatsonHeardMsg extends WatsonCompletionMsg
  final case class PersonUtteranceMsg(utterance : String, personName : String)
      extends WatsonHeardMsg
  case object SilenceMsg
      extends WatsonHeardMsg
  case object SpeakerSoundFinishedMsg
      extends WatsonCompletionMsg

  case object Active extends State

  final case class Partner(
    actorRef : ActorRef, voice : String,
    background : Option[Process] = None)
      extends Data
}
import IntercomActor._

class IntercomActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val unpaired = ActorRef.noSender

  private val startTime = readClockTime

  private var watsonOpt : Option[ActorRef] = None

  private def isWatsonEnabled =
    !settings.WatsonTts.user.isEmpty && !settings.WatsonStt.user.isEmpty

  override def preStart()
  {
    if (isWatsonEnabled) {
      watsonOpt = Some(context.actorOf(
        Props(classOf[WatsonActor]), "watsonActor"))
    }
    log.info("IntercomActor started")
  }

  override def postStop()
  {
    log.info("IntercomActor stopped")
  }

  startWith(Active, Partner(unpaired, VOICE_NONE))

  when(Active) {
    case Event(UptimeRequestMsg, _) => {
      val checkTime = readClockTime
      val diff = Seconds.secondsBetween(startTime, checkTime)
      sender ! UptimeResponseMsg(diff.getSeconds)
      stay
    }
    case Event(PairRequestMsg(voice), Partner(oldPartner, _, bg)) => {
      if (sender == oldPartner) {
        oldPartner ! ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED)
        stay
      } else if (oldPartner == unpaired) {
        sender ! PairAcceptedMsg
        stay using Partner(sender, voice, bg)
      } else {
        sender ! BusyMsg
        stay
      }
    }
    case Event(PairPreemptMsg(voice), Partner(oldPartner, _, bg)) => {
      if (oldPartner != unpaired) {
        oldPartner ! PreemptionDisconnectMsg
      }
      sender ! PairAcceptedMsg
      stay using Partner(sender, voice, bg)
    }
    case Event(UnpairMsg, Partner(partner, _, bg)) => {
      if (sender == partner) {
        context.parent ! UnpairedMsg
        stay using Partner(unpaired, VOICE_NONE, bg)
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerUtteranceMsg(utterance), Partner(partner, voice, _)) => {
      if (sender == partner) {
        watsonOpt match {
          case Some(watson) => {
            watson ! WatsonActor.SpeechSayMsg(utterance, voice)
          }
          case _ => {
            log.info("Unable to say '" + utterance + "' using voice " + voice)
            partner ! SpeakerSoundFinishedMsg
          }
        }
        stay
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerListenMsg(newPersonName), Partner(partner, _, _)) => {
      if (sender == partner) {
        context.parent ! ListeningStartedMsg
        watsonOpt match {
          case Some(watson) => {
            watson ! WatsonActor.SpeechListenMsg(newPersonName)
          }
          case _ => {
            partner ! SilenceMsg
          }
        }
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_LISTEN_WITHOUT_PAIR)
      }
      stay
    }
    case Event(RingtoneMsg, Partner(partner, _, _)) => {
      if (partner == unpaired) {
        log.info("Ring ring")
        val command = settings.Speaker.ringtoneCommand
        if (!command.isEmpty) {
          command.!
        }
        sender ! SpeakerSoundFinishedMsg
      } else if (partner == sender) {
        sender ! ProtocolErrorMsg(PROTOCOL_RING_WHILE_PAIRED)
      } else {
        sender ! BusyMsg
      }
      stay
    }
    case Event(DoorbellMsg, Partner(partner, _, _)) => {
      log.info("Ding dong")
      val command = settings.Speaker.doorbellCommand
      if (!command.isEmpty) {
        command.!
      }
      sender ! SpeakerSoundFinishedMsg
      stay
    }
    case Event(StartAudioFileMsg(file, loop),
      Partner(partner, voice, background)) =>
    {
      background match {
        case Some(process) => {
          process.destroy
        }
        case _ =>
      }
      val command = {
        if (loop) {
          settings.Speaker.loopFileCommand
        } else {
          settings.Speaker.playFileCommand
        }
      }
      val process = command.format(file).run
      stay using Partner(partner, voice, Some(process))
    }
    case Event(StopAudioFileMsg, Partner(partner, voice, background)) => {
      background match {
        case Some(process) => {
          process.destroy
          stay using Partner(partner, voice)
        }
        case _ => {
          stay
        }
      }
    }
    case Event(msg : WatsonCompletionMsg, Partner(partner, _, _)) => {
      msg match {
        case heardMsg : WatsonHeardMsg => {
          context.parent ! ListeningDoneMsg
        }
        case _ =>
      }
      // forward it on to partner
      if (partner != unpaired) {
        partner ! msg
      }
      stay
    }
  }

  initialize()
}
