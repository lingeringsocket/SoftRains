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

class RecordingTopic(sender : HomeResident, recipient : HomeResident)
    extends ConversationTopic
{
  object RecordingState extends Enumeration
  {
    type RecordingState = Value
    val INTRO, RECORD, CONFIRM, CONFUSED,
      PLAYBACK, SENT, CANCELED, DONE = Value
  }
  import RecordingState._

  var state = INTRO

  var audioFile : Option[String] = None

  override def getPriority() = ASAP

  override def isInProgress() : Boolean = (state != DONE)

  override def produceUtterance(context : ConversationContext) =
    delegateToProduceMessage(context)

  override def produceMessage(context : ConversationContext) =
  {
    state match {
      case INTRO => {
        state = RECORD
        Some(IntercomActor.PartnerUtteranceMsg(
          "Okay, please tell me your message now."))
      }
      case CONFIRM => {
        Some(IntercomActor.PartnerUtteranceMsg(
          "Should I play back your message, " +
            "or send it now?"))
      }
      case PLAYBACK => {
        state = CONFIRM
        Some(IntercomActor.SpeakerSoundSeqMsg(Seq(
          IntercomActor.PlayAudioFileMsg(audioFile.getOrElse("hodor.mp3")),
          IntercomActor.PartnerUtteranceMsg(
            "Do you want to play it back again, send it, cancel, "
              + "or ree ree cord?"))))
      }
      case SENT => {
        state = DONE
        Some(IntercomActor.PartnerUtteranceMsg(
          "Bombs away!"))
      }
      case CANCELED => {
        state = DONE
        Some(IntercomActor.PartnerUtteranceMsg(
          "OK, never mind then."))
      }
      case CONFUSED => {
        state = CONFIRM
        Some(IntercomActor.PartnerUtteranceMsg(
          "I beg your pardon?  I need to know whether you want " +
            "to play your message back, or send it, or ree ree cord it, " +
            "or cancel?"))
      }
      case _ => None
    }
  }

  def consumeUtterance(
    utterance : String, personName : String, context : ConversationContext) =
  {
    val inputSplit = ContainsTopicMatcher.splitWords(utterance.toLowerCase)
    state match {
      case RECORD => {
        val utterances = context.getUtterances
        if (utterances.isEmpty) {
          audioFile = None
        } else {
          audioFile = context.getUtterances.last.audioFile
        }
        state = CONFIRM
      }
      case CONFIRM => {
        if (ContainsTopicMatcher.matchPhrases(
          inputSplit, Seq("play", "back", "playback")))
        {
          state = PLAYBACK
        } else if (ContainsTopicMatcher.matchPhrases(
          inputSplit, Seq("send", "deliver")))
        {
          if (!audioFile.isEmpty) {
            context.createNotification(PendingNotification(
              recipient,
              if (recipient.name == sender.name) {
                "You left yourself a message"
              } else {
                "Here is a message from " + sender.name + "."
              },
              audioFile,
              CommunicationPriority.ASAP,
              context.getCurrentTime
            ))
          }
          state = SENT
        } else if (ContainsTopicMatcher.matchPhrases(
          inputSplit, Seq("cancel")))
        {
          state = CANCELED
        } else if (ContainsTopicMatcher.matchPhrases(
          inputSplit, Seq("record")))
        {
          state = INTRO
        } else {
          state = CONFUSED
        }
      }
      case _ =>
    }
  }
}
