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
import softrains.intercom._

import CommunicationPriority._

class EchoTopic
    extends ConversationTopic
{
  private var done = false
  private var echo = ""

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = !done

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  override def produceMessage(context : ConversationContext) =
  {
    if (echo.isEmpty) {
      Some(
        IntercomActor.PartnerUtteranceMsg("Polly wants a cracker!"))
    } else {
      if (echo.contains("terminate") || echo.contains("bye")) {
        done = true
        None
      } else {
        Some(IntercomActor.PartnerUtteranceMsg(echo))
      }
    }
  }

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
  {
    echo = utterance.toLowerCase
  }

  override def consumeAlternatives(alternatives : Seq[String])
  {
    if (alternatives.size > 1) {
      echo = alternatives.map(_.toLowerCase).mkString(".  or maybe ") +
        ".  that's all!"
    }
  }
}
