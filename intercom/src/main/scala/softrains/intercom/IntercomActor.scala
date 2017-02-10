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

import java.io._

object IntercomActor
{
  val PROTOCOL_ALREADY_PAIRED = "already paired"

  val PROTOCOL_UNPAIR_WITHOUT_PAIR = "cannot unpair without pairing first"

  val PROTOCOL_UTTERANCE_WITHOUT_PAIR = "must be paired before utterance"

  val PROTOCOL_LISTEN_WITHOUT_PAIR = "must be paired before listening"

  val PROTOCOL_RING_WHILE_PAIRED = "cannot ring phone while paired"

  val VOICE_DEFAULT = "en-US_AllisonVoice"

  sealed trait State
  sealed trait Data

  // received messages
  trait SpeakerSoundMsg extends PeripheralMsg
  final case class SetObserverMsg(actor : ActorRef)
      extends PeripheralMsg
  case object PairRequestMsg
      extends PeripheralMsg
  case object PairPreemptMsg
      extends PeripheralMsg
  final case class PartnerUtteranceMsg(utterance : String, voice : String = "")
      extends SpeakerSoundMsg
  final case class PartnerListenMsg(
    personName : String = "", identifyVoice : Boolean = false)
      extends PeripheralMsg
  case object UnpairMsg
      extends PeripheralMsg
  case object PreWakeMsg
      extends PeripheralMsg
  case object UptimeRequestMsg
      extends PeripheralMsg
  final case class InitializeAlexaMsg(alexaActor : ActorRef)
      extends PeripheralMsg
  case object WakeAlexaMsg
      extends SpeakerSoundMsg
  case object AlexaFinishedMsg
      extends PeripheralMsg
  case object RingtoneMsg
      extends SpeakerSoundMsg
  case object DoorbellMsg
      extends SpeakerSoundMsg
  final case class PlayAudioFileMsg(audioFile : String)
      extends SpeakerSoundMsg
  final case class StartAudioFileMsg(audioFile : String, loop : Boolean)
      extends SpeakerSoundMsg
  case object StopAudioFileMsg
      extends SpeakerSoundMsg
  // not actually received directly
  final case class SpeakerSoundSeqMsg(
    seq : Seq[SpeakerSoundMsg])
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
  case object WokeUpMsg
      extends PeripheralMsg
  case object FellAsleepMsg
      extends PeripheralMsg
  case object PairedMsg
      extends PeripheralMsg
  case object UnpairedMsg
      extends PeripheralMsg
  trait ListeningNotificationMsg
      extends PeripheralMsg
  case object ListeningStartedMsg
      extends ListeningNotificationMsg
  case object ListeningDoneMsg
      extends ListeningNotificationMsg

  // forwarded messages (from watson to partner)
  trait WatsonCompletionMsg extends PeripheralMsg
  trait WatsonHeardMsg extends WatsonCompletionMsg
  final case class PersonUtteranceMsg(
    utterance : String, personName : String, fileOpt : Option[String] = None)
      extends WatsonHeardMsg
  case object SilenceMsg
      extends WatsonHeardMsg
  final case class SpeakerSoundFinishedMsg(fileOpt : Option[String] = None)
      extends WatsonCompletionMsg

  case object Asleep extends State
  case object Awake extends State

  final case class Partner(
    actorRef : ActorRef, voice : String,
    background : Option[Process] = None)
      extends Data
}
import IntercomActor._

class IntercomActor extends LoggingFSM[State, Data]
{
  private val settings = SoftRainsActorSettings(context)

  private val sleepTimeout = settings.Speaker.sleepTimeout

  private val unpaired = ActorRef.noSender

  private val startTime = readClockTime

  private var watsonOpt : Option[ActorRef] = None

  private var alexaOpt : Option[ActorRef] = None

  private var observer = context.parent

  private def isWatsonEnabled =
    !settings.WatsonTts.user.isEmpty && !settings.WatsonStt.user.isEmpty

  override def preStart()
  {
    if (isWatsonEnabled) {
      val watsonActor = context.actorOf(
        Props(classOf[WatsonActor]), "watsonActor")
      watsonActor ! WatsonActor.SpeechSayMsg("Yabba Dabba Doo!", VOICE_DEFAULT)
      watsonOpt = Some(watsonActor)
    }
    log.info("IntercomActor started")
  }

  override def postStop()
  {
    log.info("IntercomActor stopped")
  }

  startWith(Asleep, Partner(unpaired, VOICE_DEFAULT))

  override def receive = ({
    case SetObserverMsg(actor) => {
      observer = actor
    }
    // we don't want uptime pings to keep us awake
    case UptimeRequestMsg => {
      val checkTime = readClockTime
      val diff = Seconds.secondsBetween(startTime, checkTime)
      sender ! UptimeResponseMsg(diff.getSeconds)
    }
  }: Receive) orElse super.receive

  private def watsonSay(utterance : String, voice : String, partner : ActorRef)
  {
    watsonOpt match {
      case Some(watson) => {
        watson ! WatsonActor.SpeechSayMsg(utterance, voice)
      }
      case _ => {
        log.info("Unable to say '" + utterance + "' using voice " + voice)
        partner ! SpeakerSoundFinishedMsg()
      }
    }
  }

  private def getAbsoluteFile(fileName : String) =
  {
    val file = new File(fileName)
    if (file.isAbsolute) {
      file
    } else {
      new File(settings.Speaker.soundPath, fileName)
    }
  }

  when(Asleep) {
    case Event(msg, _) => {
      observer ! WokeUpMsg
      val command = settings.Speaker.wakeCommand
      if (!command.isEmpty) {
        command.!
      }
      self.forward(msg)
      goto(Awake)
    }
  }

  when(Awake, stateTimeout = sleepTimeout) {
    case Event(PreWakeMsg, _) => {
      stay
    }
    case Event(StateTimeout, _) => {
      observer ! FellAsleepMsg
      val command = settings.Speaker.sleepCommand
      if (!command.isEmpty) {
        command.!
      }
      goto(Asleep)
    }
    case Event(InitializeAlexaMsg(alexaActor), _) => {
      alexaOpt = Some(alexaActor)
      stay
    }
    case Event(PairRequestMsg, Partner(oldPartner, voice, bg)) => {
      if (sender == oldPartner) {
        oldPartner ! ProtocolErrorMsg(PROTOCOL_ALREADY_PAIRED)
        stay
      } else if (oldPartner == unpaired) {
        observer ! PairedMsg
        sender ! PairAcceptedMsg
        stay using Partner(sender, voice, bg)
      } else {
        sender ! BusyMsg
        stay
      }
    }
    case Event(PairPreemptMsg, Partner(oldPartner, voice, bg)) => {
      if (oldPartner != unpaired) {
        oldPartner ! PreemptionDisconnectMsg
      }
      sender ! PairAcceptedMsg
      stay using Partner(sender, voice, bg)
    }
    case Event(UnpairMsg, Partner(partner, voice, bg)) => {
      if (sender == partner) {
        observer ! UnpairedMsg
        stay using Partner(unpaired, voice, bg)
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UNPAIR_WITHOUT_PAIR)
        stay
      }
    }
    case Event(PartnerUtteranceMsg(utterance, newVoice),
      Partner(partner, oldVoice, bg)) =>
    {
      if (sender == partner) {
        val voice = {
          if (newVoice.isEmpty) {
            oldVoice
          } else {
            newVoice
          }
        }
        watsonSay(utterance, voice, partner)
        stay using Partner(partner, voice, bg)
      } else {
        sender ! ProtocolErrorMsg(PROTOCOL_UTTERANCE_WITHOUT_PAIR)
        stay
      }
    }
    case Event(
      PartnerListenMsg(personName, identifyVoice),
      Partner(partner, _, _)) =>
    {
      if (sender == partner) {
        observer ! ListeningStartedMsg
        watsonOpt match {
          case Some(watson) => {
            watson ! WatsonActor.SpeechListenMsg(
              personName, identifyVoice)
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
        sender ! SpeakerSoundFinishedMsg()
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
      sender ! SpeakerSoundFinishedMsg()
      stay
    }
    case Event(StartAudioFileMsg(fileName, loop),
      Partner(partner, voice, background)) =>
    {
      background.foreach(_.destroy)
      val command = {
        if (loop) {
          settings.Speaker.loopFileCommand
        } else {
          settings.Speaker.playFileCommand
        }
      }
      val absoluteFile = getAbsoluteFile(fileName)
      val process = command.format(absoluteFile).run
      sender ! SpeakerSoundFinishedMsg()
      stay using Partner(partner, voice, Some(process))
    }
    case Event(PlayAudioFileMsg(fileName),
      Partner(partner, voice, background)) =>
    {
      background.foreach(_.destroy)
      val command = settings.Speaker.playFileCommand
      val absoluteFile = getAbsoluteFile(fileName)
      command.format(absoluteFile).!
      sender ! SpeakerSoundFinishedMsg()
      stay using Partner(partner, voice, None)
    }
    case Event(StopAudioFileMsg, Partner(partner, voice, background)) => {
      sender ! SpeakerSoundFinishedMsg()
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
    case Event(WakeAlexaMsg, _) => {
      alexaOpt.foreach(_ ! WakeAlexaMsg)
      stay
    }
    case Event(AlexaFinishedMsg, Partner(partner, voice, _)) => {
      watsonSay("Did you enjoy chatting with Alexa?", voice, partner)
      stay
    }
    case Event(msg : ListeningNotificationMsg, _) => {
      // forward it on to parent
      observer ! msg
      stay
    }
    case Event(msg : WatsonCompletionMsg, Partner(partner, _, _)) => {
      msg match {
        case _ : WatsonHeardMsg => {
          observer ! ListeningDoneMsg
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
