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
      val notification = "Good morning!"
      val processor = new NotificationConversationProcessor(notification)
      processor.produceUtterance() must be equalTo(Some(notification))
      processor.produceUtterance() must be equalTo(None)
    }

    "anticipate good morning" in
    {
      val pikachu = new HomeResident("Pikachu")
      val greeting = new DailyGreeting(pikachu)
      greeting.isReady must beTrue
      greeting.isExpired must beFalse
      greeting.isConversational must beFalse
      greeting.getPriority must be equalTo(CommunicationPriority.ASAP)
      val processor = greeting.startCommunication
      processor.produceUtterance() must be equalTo(
        Some("Good morning, Pikachu!"))
      processor.produceUtterance() must be equalTo(None)
    }
  }
}
