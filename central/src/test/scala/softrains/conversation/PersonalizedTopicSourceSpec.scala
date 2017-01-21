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

import org.joda.time._

import akka.actor._

class PersonalizedTopicSourceSpec extends AkkaActorSpecification
{
  // database state is shared, so we need isolation
  sequential

  "PersonalizedTopicSource" should
  {
    "generate initial greetings" in new AkkaActorExample
    {
      val context = new PersonalizedConversationContext(system, settings)
      val source = new PersonalizedTopicSource

      // 2017-Jan-20 (Friday)
      // 10am
      context.setTime(new DateTime(2017, 1, 20, 10, 0, 0))
      source.generateGreeting(context) must be equalTo
        "Good morning, Stranger! Thank God it's Friday!"

      // 1pm
      context.setTime(new DateTime(2017, 1, 20, 13, 0, 0))
      source.generateGreeting(context) must be equalTo
        "Good afternoon, Stranger! Thank God it's Friday!"

      // 7pm
      context.setTime(new DateTime(2017, 1, 20, 19, 0, 0))
      source.generateGreeting(context) must be equalTo
      "Good evening, Stranger! Thank God it's Friday!"

      // 2017-Jan-21 (Saturday)
      context.setTime(new DateTime(2017, 1, 21, 10, 0, 0))
      source.generateGreeting(context) must be equalTo
        "Good morning, Stranger! Happy Saturday!"

      // 2017-Jan-23 (Monday)
      context.setTime(new DateTime(2017, 1, 23, 10, 0, 0))
      source.generateGreeting(context) must be equalTo
        "Good morning, Stranger! Ready for another week?"
    }

    "generate repeat greeting" in new AkkaActorExample
    {
      val context = new PersonalizedConversationContext(system, settings)
      val source = new PersonalizedTopicSource

      val prevTime = new DateTime(2017, 1, 20, 10, 0, 0)
      val lastTime = new DateTime(2017, 1, 20, 10, 0, 1)
      val nearTime = new DateTime(2017, 1, 20, 10, 15, 0)
      val farTime = new DateTime(2017, 1, 20, 16, 15, 0)

      val db = context.getDatabase
      val transcript = db.save(ConversationTranscript(lastTime))
      context.setTranscript(transcript)

      context.setTime(nearTime)
      source.generateGreeting(context) must be equalTo
        "Good morning, Stranger! Thank God it's Friday!"

      context.getPreviousUtteranceFor("Stranger") must beNone

      db.save(ConversationUtterance(
          transcript, prevTime, "Stranger", "Yo"))
      db.save(ConversationUtterance(
          transcript, lastTime, "Stranger", "Dawg"))

      context.getPreviousUtteranceFor("Stranger").map(_.text) must
        beSome("Yo")

      source.generateGreeting(context) must be equalTo
        "Hello again, Stranger!"

      context.setTime(farTime)
      source.generateGreeting(context) must be equalTo
        "Good afternoon, Stranger! How is your day going?"
    }
  }

  private class PersonalizedConversationContext(
    system : ActorSystem, settings : SoftRainsSettings)
      extends ConversationContext
  {
    private val db = new CentralDb(settings)

    private var transcriptOpt :
        Option[ConversationTranscript with sorm.Persisted] = None

    private var currentTime = readClockTime

    override def getActorSystem = system

    override def getSettings = settings

    override def getDatabase = db

    override def getCurrentTime = currentTime

    override def getTranscript = transcriptOpt

    def setTime(newTime : DateTime)
    {
      currentTime = newTime
    }

    def setTranscript(
      newTranscript : ConversationTranscript with sorm.Persisted)
    {
      transcriptOpt = Some(newTranscript)
    }
  }
}
