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

import softrains.central._

import org.specs2.mutable._

class ConversationSpec extends Specification
{
  "ConversationProcessor" should
  {
    "say good morning" in
    {
      val pikachu = new HomeResident("Pikachu")
      val greeting = new DailyGreeting(pikachu)
      greeting.getPriority must be equalTo(CommunicationPriority.ASAP)
      greeting.isInProgress must beFalse
      greeting.produceUtterance() must be equalTo(
        Some("Good morning, Pikachu!"))
      greeting.produceUtterance() must be equalTo(None)
    }

    "dispatch known person" in
    {
      val personName = "Ash"
      val topicSource = new SequentialTopicSource(Seq(
        new WarningTopic("The house is on fire!")
      ))
      val dispatcher = new TopicDispatcher(topicSource, personName)
      dispatcher.produceUtterance() must be equalTo(
        Some("Hello, Ash.  How are you?"))
      dispatcher.consumeUtterance("Very well, thank you.")
      dispatcher.produceUtterance() must be equalTo(
        Some("The house is on fire!"))
      dispatcher.consumeUtterance("Thanks for letting me know.")
      dispatcher.produceUtterance() must be equalTo(
        Some("So, what is on your mind?"))
      dispatcher.consumeUtterance("Goodbye")
      dispatcher.produceUtterance() must be equalTo(
        Some("Talk to you later!"))
      dispatcher.isInProgress must beFalse
    }

    "dispatch without pleasantries" in
    {
      val personName = "Ash"
      val topicSource = new SequentialTopicSource(Seq.empty)
      val dispatcher = new TopicDispatcher(topicSource, personName)
      dispatcher.produceUtterance() must be equalTo(
        Some("Hello, Ash.  How are you?"))
      dispatcher.consumeUtterance("Thanks a million.")
      dispatcher.produceUtterance() must be equalTo(
        Some("You are very welcome!"))
      dispatcher.consumeUtterance("Goodbye")
      dispatcher.produceUtterance() must be equalTo(
        Some("Talk to you later!"))
      dispatcher.isInProgress must beFalse
    }

    "dispatch unknown voice" in
    {
      val topicSource = new SequentialTopicSource(Seq(
        new WarningTopic("You just won the lottery!")))
      val dispatcher = new TopicDispatcher(topicSource)
      dispatcher.getPriority must be equalTo(
        CommunicationPriority.ONLY_IF_NOT_BUSY)
      dispatcher.produceUtterance() must be equalTo(
        Some("Who goes there?"))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance(
        "whatevs")
      dispatcher.produceUtterance() must be equalTo(
        Some("Sorry, I don't recognize your voice."))
      dispatcher.isInProgress must beFalse
      dispatcher.produceUtterance() must be equalTo(None)
    }

    "dispatch to no topic" in
    {
      val topicSource = new SequentialTopicSource(Seq.empty)
      val dispatcher = new TopicDispatcher(topicSource)
      val context = new ConversationSubContext(NullConversationContext)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("Who goes there?"))
      dispatcher.consumeUtterance(
        "Your worst enemy", "Voldemort", context)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("So, what is on your mind?"))
    }

    "dispatch to daily greeting" in
    {
      val frodo = new HomeResident("Frodo")
      val greeting = new DailyGreeting(frodo)
      val topicSource = new SequentialTopicSource(Seq(greeting))
      val dispatcher = new TopicDispatcher(topicSource)
      val context = new ConversationSubContext(NullConversationContext)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("Who goes there?"))
      dispatcher.consumeUtterance(
        "The Ring-bearer", frodo.name, context)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("Good morning, Frodo!"))
      dispatcher.isInProgress must beTrue
      dispatcher.produceUtterance(context) must be equalTo(
        Some("So, what is on your mind?"))
      dispatcher.consumeUtterance(
        "Thank you very much", frodo.name, context)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("You are very welcome!"))
    }

    "dispatch to echo loop followed by voice identifier" in
    {
      val echoLoop = new EchoLoop
      val bert = HomeResident("Bert")
      val ernie = HomeResident("Ernie")
      val voiceIdentifier = new VoiceIdentifier(Seq(
        bert, ernie))
      val topicSource = new SequentialTopicSource(Seq(
        echoLoop, voiceIdentifier))
      val dispatcher = new TopicDispatcher(topicSource)
      dispatcher.produceUtterance() must be equalTo(
        Some("Who goes there?"))
      dispatcher.consumeUtterance(
        "It's me, Bert", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("Polly wants a cracker!"))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("here you go.", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("here you go."))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("want another one?", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("want another one?"))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("terminate", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("Bert, please say, the quick brown fox jumped over the lazy dog."))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("blah blah blah", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("Now, Ernie, you say the same sentence."))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("blah blah blah", ernie.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("Now someone say anything and " +
          "I will try to identify the speaker."))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("blah blah blah", ernie.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("I heard Ernie say, blah blah blah.  Try another?"))
      dispatcher.isInProgress must beTrue
      dispatcher.consumeUtterance("I want my rubber ducky", bert.name)
      dispatcher.produceUtterance() must be equalTo(
        Some("I heard Bert say, I want my rubber ducky.  Try another?"))
      dispatcher.isInProgress must beTrue
    }

    "answer some personal questions" in
    {
      val resident = new HomeResident("John")
      val topicSource = new SequentialTopicSource(Seq.empty)
      val dispatcher = new TopicDispatcher(topicSource, resident.name)
      val context = new ConversationSubContext(NullConversationContext)
      dispatcher.produceUtterance(context) must be equalTo(
        Some("Hello, John.  How are you?"))
      dispatcher.consumeUtterance(
        "Who are you?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "My name is Allison")
      dispatcher.consumeUtterance(
        "Where are you?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "I live in a magical cloud palace")
      dispatcher.consumeUtterance(
        "Who am I?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "I am fairly sure you are John")
      dispatcher.consumeUtterance(
        "Who is Donald Trump?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "I am not sure who or what")
      dispatcher.consumeUtterance(
        "Where am I?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "Silly human")
      dispatcher.consumeUtterance(
        "Who is Sujin?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "Sujin is my favorite")
      dispatcher.consumeUtterance(
        "Who is she?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "Sujin is my favorite")
      dispatcher.consumeUtterance(
        "Where is he?", resident.name, context)
      dispatcher.produceUtterance(context).get must startWith(
        "I am not sure who or what")
    }
  }
}
