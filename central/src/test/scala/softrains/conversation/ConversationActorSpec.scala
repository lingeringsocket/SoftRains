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
import akka.testkit._

import scala.collection._

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
      expectMsg(PartnerUtteranceMsg(utterance2))
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
      val utterance2 = "Please wait half a second,,,"
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
      expectMsg(PartnerUtteranceMsg(utterance2))
      actor ! SpeakerSoundFinishedMsg()
      expectMsg(PartnerUtteranceMsg(utterance3, "en-GB_KateVoice"))
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
  }
}
