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
package softrains.intercom

import softrains.base._

import akka.actor._

import scala.sys.process._

import org.joda.time.Seconds

import java.io._
import java.net._

object IntercomActor
{
  val PROTOCOL_ALREADY_PAIRED = "already paired"

  val PROTOCOL_UNPAIR_WITHOUT_PAIR = "cannot unpair without pairing first"

  val PROTOCOL_UTTERANCE_WITHOUT_PAIR = "must be paired before utterance"

  val PROTOCOL_LISTEN_WITHOUT_PAIR = "must be paired before listening"

  val PROTOCOL_RING_WHILE_PAIRED = "cannot ring phone while paired"

  val VOICE_DEFAULT = "en-US_AllisonVoice"

  private val ps = IntercomPhraseSet(DefaultPhraseContext)

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
  final case class SystemUtteranceMsg(utterance : String, voice : String = "")
      extends SpeakerSoundMsg
  final case class PartnerListenMsg(
    personName : String = "", identifyVoice : Boolean = false)
      extends PeripheralMsg
  case object UnpairMsg
      extends PeripheralMsg
  case object PreWakeMsg
      extends PeripheralMsg
  case object VolumeUpMsg
      extends PeripheralMsg
  case object VolumeDownMsg
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
  final case class RebootMsg(soft : Boolean = true)
      extends SpeakerSoundMsg
  final case class PlayAudioFileMsg(audioFile : String)
      extends SpeakerSoundMsg
  final case class StartAudioFileMsg(
    audioFile : String, loop : Boolean, daemonize : Boolean = false)
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
  final case class ReadyMsg(name : String)
      extends PeripheralMsg
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
  case object ConversationRequestedMsg
      extends ListeningNotificationMsg
  case object ListeningStartedMsg
      extends ListeningNotificationMsg
  case object ListeningDoneMsg
      extends ListeningNotificationMsg

  // forwarded messages (from watson to partner)
  trait WatsonCompletionMsg extends PeripheralMsg
  trait WatsonHeardMsg extends WatsonCompletionMsg
  final case class PersonUtteranceMsg(
    alternatives : Seq[String],
    personName : String,
    fileOpt : Option[String])
      extends WatsonHeardMsg
  {
    def utterance = alternatives.head
  }
  case object PersonUtteranceMsg {
    def apply(utterance : String, personName : String,
      fileOpt : Option[String] = None) =
    {
      new PersonUtteranceMsg(Seq(utterance), personName, fileOpt)
    }
  }
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
    val command = settings.Speaker.killAudioCommand
    if (!command.isEmpty) {
      command.!
    }
    if (isWatsonEnabled) {
      val watsonActor = context.actorOf(
        Props(classOf[WatsonActor]), "watsonActor")
      if (settings.Intercom.announceReady) {
        watsonActor ! WatsonActor.SpeechSayMsg(
          ps.ready, VOICE_DEFAULT)
      }
      watsonOpt = Some(watsonActor)
    }
    val readyUrl = settings.Intercom.readyUrl
    if (!readyUrl.isEmpty) {
      val httpConsumer = new HttpConsumer(context.system)
      httpConsumer.fetchString(readyUrl) {result => }
      // note that we intentionally skip httpConsumer.ensureSuccess
      // since central may not currently be up
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

  private def getAbsoluteFile(fileName : String) : String =
  {
    if (fileName.startsWith("http")) {
      return URLDecoder.decode(fileName, "UTF-8")
    }
    val file = new File(fileName)
    val soundPath = settings.Speaker.soundPath
    if (file.isAbsolute || soundPath.getName.isEmpty) {
      file.toString
    } else {
      new File(soundPath, fileName).toString
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
    case Event(SystemUtteranceMsg(utterance, systemVoice),
      Partner(partner, oldVoice, bg)) =>
    {
      val voice = {
        if (systemVoice.isEmpty) {
          "en-US_LisaVoice"
        } else {
          systemVoice
        }
      }
      watsonSay(utterance, voice, unpaired)
      stay
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
    case Event(VolumeUpMsg, _) => {
      val command = settings.Speaker.volumeUpCommand
      if (!command.isEmpty) {
        command.!
      }
      stay
    }
    case Event(VolumeDownMsg, _) => {
      val command = settings.Speaker.volumeDownCommand
      if (!command.isEmpty) {
        command.!
      }
      stay
    }
    case Event(RebootMsg(soft), _) => {
      observer ! UnpairedMsg
      sender ! SpeakerSoundFinishedMsg()
      Thread.sleep(5000)
      val command = settings.Intercom.restartCommand
      if (!command.isEmpty) {
        command.!
      }
      stay
    }
    case Event(StartAudioFileMsg(fileName, loop, daemonize),
      Partner(partner, voice, background)) =>
    {
      if (!daemonize) {
        background.foreach(_.destroy)
      }
      val command = {
        if (loop) {
          settings.Speaker.loopFileCommand
        } else {
          settings.Speaker.playFileCommand
        }
      }
      val absoluteFile = getAbsoluteFile(fileName)
      val formattedCommand = command.format(absoluteFile)
      val newData = {
        if (daemonize) {
          val scriptCommand = "screen -d -m " + formattedCommand
          scriptCommand.!
          Partner(partner, voice, background)
        } else {
          val process = formattedCommand.run
          Partner(partner, voice, Some(process))
        }
      }
      sender ! SpeakerSoundFinishedMsg()
      stay using newData
    }
    case Event(PlayAudioFileMsg(fileName), _) => {
      val command = settings.Speaker.playFileCommand
      val absoluteFile = getAbsoluteFile(fileName)
      command.format(absoluteFile).!
      sender ! SpeakerSoundFinishedMsg()
      stay
    }
    case Event(StopAudioFileMsg, Partner(partner, voice, background)) => {
      val command = settings.Speaker.killAudioCommand
      if (!command.isEmpty) {
        command.!
      }
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
      watsonSay(ps.alexaDone, voice, partner)
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
