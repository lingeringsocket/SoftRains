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
package softrains.central

import softrains.intercom._
import softrains.conversation._

import akka.io._
import akka.actor._
import akka.util._
import java.net._

class CentralTelnetConnection(conversationActor : ActorRef, peer : ActorRef)
    extends Actor with ActorLogging
{
  import Tcp._
  import IntercomActor._

  private def say(voice : String, utterance : String)
  {
    peer ! Write(ByteString("[" + voice + "]  " + utterance + "\n"))
  }

  def receive =
  {
    case Received(data) => {
      val utterance = data.decodeString("utf-8").trim
      conversationActor ! PersonUtteranceMsg(utterance, "")
    }
    case PeerClosed => {
      log.info("Telnet connection closed by peer")
      context.stop(self)
    }
    case PairRequestMsg => {
      sender ! PairAcceptedMsg
    }
    case UnpairMsg => {
      log.info("Telnet connection closed by central")
      context.stop(self)
    }
    case PartnerUtteranceMsg(utterance, voice) => {
      say(voice, utterance)
      sender ! SpeakerSoundFinishedMsg()
    }
    case SystemUtteranceMsg(utterance, voice) => {
      say(voice, utterance)
      sender ! SpeakerSoundFinishedMsg()
    }
  }
}

class CentralTelnetServer(central : CentralService)
    extends Actor with ActorLogging
{
  import Tcp._
  import context.system

  private val settings = central.getSettings

  private var nextConversationId = 0

  IO(Tcp) ! Bind(
    self, new InetSocketAddress(
      settings.Telnet.address, settings.Telnet.port))

  def receive =
  {
    case b @ Bound(localAddress) => {
      log.info("Telnet server listening at " + localAddress)
    }

    case CommandFailed( _: Bind ) => {
      log.info("Telnet server setup failed.")
      context.stop(self)
    }

    case c @ Connected(remote, local) => {
      log.info(
        "Telnet connection received from hostname: " + remote.getHostName +
          " address: " + remote.getAddress.toString)
      val conversationProps = Props(
        classOf[ConversationActor],
        central.getDatabase)
      val conversationSpec = "telnetConversationActor" + nextConversationId
      nextConversationId += 1
      val conversationActor =
        system.actorOf(conversationProps, conversationSpec)
      val connection = sender
      val handler = context.actorOf(
        Props(classOf[CentralTelnetConnection], conversationActor, connection))
      val topicSource = new SequentialTopicSource(Seq.empty)
      val dispatcher = new TopicDispatcher(topicSource, "", "At your service!")
      conversationActor ! ConversationActor.ActivateMsg(
        dispatcher,
        handler,
        ConversationPartner.MICHAEL)
      connection ! Register(handler)
    }
  }
}
