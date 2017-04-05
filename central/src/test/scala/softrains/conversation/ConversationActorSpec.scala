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
import akka.testkit._

import scala.collection._

import org.joda.time._

class ConversationActorSpec
    extends AkkaActorSpecification with DateTimeOrderingImplicit
{
  import ConversationActor._
  import IntercomActor._

  // database state is shared, so we need isolation
  sequential

  private val typhlosion = new HomeResident("Typhlosion")

  "ConversationActor" should
  {
    "have a one-way conversation" in new AkkaActorExample
    {
      val utterance0 = "Good morning, Typhlosion!"
      val db = new CentralDb(settings)

      // use TestActorRef for synchronous end transition
      val actor = TestActorRef(new ConversationActor(db))
      val greeting = new DailyGreeting(typhlosion)
      actor ! ActivateMsg(greeting, self)
      expectMsg(PairRequestMsg)
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg(utterance0))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(UnpairMsg)

      db.query[ConversationTranscript].fetch.size must be equalTo 1
      db.query[ConversationUtterance].fetch.size must be equalTo 1

      val transcript = db.query[ConversationTranscript].fetchOne.get
      transcript.endTime must beSome
      transcript.endTime.get must beGreaterThan(transcript.startTime)

      val utterance = db.query[ConversationUtterance].fetchOne.get
      utterance.startTime must be equalTo transcript.startTime
      utterance.person must be equalTo "SoftRains"
      utterance.text must be equalTo utterance0
    }

    "have a two-way conversation" in new AkkaActorExample
    {
      val utterance0 = "So, what is on your mind?"
      val utterance1 = "Goodbye"
      val utterance2 = "Talk to you later!"

      val db = new CentralDb(settings)
      val actor = system.actorOf(Props(classOf[ConversationActor], db))
      val greeting = new PassiveTopic(typhlosion.name)
      actor ! ActivateMsg(greeting, self)
      expectMsg(PairRequestMsg)
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg(utterance0))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg("", false))
      actor ! PersonUtteranceMsg(utterance1, "")
      expectMsg(PartnerUtteranceMsg(
        utterance2, ConversationPartner.ALLISON.voiceName))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(UnpairMsg)

      db.query[ConversationTranscript].fetch.size must be equalTo 1
      db.query[ConversationUtterance].fetch.size must be equalTo 3

      val utterances =
        db.query[ConversationUtterance].order("startTime").fetch
      utterances.map(_.text).toSeq must be equalTo immutable.Seq(
        utterance0,
        utterance1,
        utterance2)
    }

    "process a compound reply" in new AkkaActorExample
    {
      val utterance0 = "So, what is on your mind?"
      val utterance1 = "I want to talk to Kate."
      val utterance2 = "I'll put you through,,,"
      val utterance3 = "Blimey, would you like to try the bangers and mash?"

      val db = new CentralDb(settings)
      val actor = system.actorOf(Props(classOf[ConversationActor], db))
      val greeting = new PassiveTopic(typhlosion.name)
      actor ! ActivateMsg(greeting, self)
      expectMsg(PairRequestMsg)
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg(utterance0))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg("", false))
      actor ! PersonUtteranceMsg(utterance1, "")
      expectMsg(PartnerUtteranceMsg(
        utterance2, ConversationPartner.ALLISON.voiceName))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerUtteranceMsg(
        utterance3, ConversationPartner.KATE.voiceName))
      actor ! SpeakerSoundFinishedMsg()

      db.query[ConversationTranscript].fetch.size must be equalTo 1
      db.query[ConversationUtterance].fetch.size must be equalTo 4

      val utterances =
        db.query[ConversationUtterance].order("startTime").fetch
      utterances.map(_.text).toSeq must be equalTo immutable.Seq(
        utterance0,
        utterance1,
        utterance2,
        utterance3)
    }

    "process a voicemail flow" in new AkkaActorExample
    {
      val utterance0 = "Hello, John.  How are you?"
      val utterance1 = "I want to leave a message for my wife."
      val utterance2 = "Okay, please tell me your message now."
      val utterance3 = "Hodor!"
      val utterance4 = "Should I play back your message, or send it now?"
      val utterance5 = "Send it."
      val utterance6 = "Bombs away!"
      val utterance7 = "Good morning, Sujin! Ready for another week?" +
        " I have some updates for you."
      val utterance8 = "Let's hear it."
      val utterance9 = "Here is a message from John."

      val db = new CentralDb(settings)

      val actor = TestActorRef(new ConversationActor(db))

      val sender = "John"
      val recipient = "Sujin"
      val filename = "hodor.mp3"

      db.save(HomeResident(sender))
      db.save(HomeResident(recipient))

      val emptySource = new SequentialTopicSource(Seq.empty)
      val senderDispatcher = new TopicDispatcher(emptySource, sender)
      actor ! ActivateMsg(senderDispatcher, self)
      expectMsg(PairRequestMsg)
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg(utterance0))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg(sender, false))
      actor ! PersonUtteranceMsg(utterance1, "")
      val voice = ConversationPartner.ALLISON.voiceName
      expectMsg(PartnerUtteranceMsg(utterance2, voice))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg(sender, false))
      actor ! PersonUtteranceMsg(utterance3, "", Some(filename))
      expectMsg(PartnerUtteranceMsg(utterance4, voice))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg(sender, false))
      actor ! PersonUtteranceMsg(utterance5, "")
      expectMsg(PartnerUtteranceMsg(utterance6, voice))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(UnpairMsg)

      val notifications = db.query[PendingNotification].fetch
      notifications.size must be equalTo 1
      val message = notifications.head
      message.resident.name must be equalTo recipient
      message.audioFile must beSome(filename)
      message.pushTime must beEmpty
      message.expirationTime must beEmpty
      message.receiveTime must beEmpty

      val recipientSource = new PersonalizedTopicSource()
      val context = new ConversationSubContext(actor.underlyingActor)
      context.setCurrentTime(new DateTime(2017, 1, 23, 10, 0, 0))
      recipientSource.preloadTopicsForPerson(
        context, recipient)
      val recipientDispatcher = new TopicDispatcher(
        recipientSource, recipient,
        recipientSource.generateGreeting(context))
      actor ! ActivateMsg(recipientDispatcher, self)
      expectMsg(PairRequestMsg)
      actor ! PairAcceptedMsg
      expectMsg(PartnerUtteranceMsg(utterance7))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerListenMsg(recipient, false))
      actor ! PersonUtteranceMsg(utterance8, "")
      expectMsg(PartnerUtteranceMsg(utterance9, voice))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PlayAudioFileMsg(filename))
      actor ! SpeakerSoundFinishedMsg()
    }
  }
}
