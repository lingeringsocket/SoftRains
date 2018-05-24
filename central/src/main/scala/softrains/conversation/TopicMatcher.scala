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

import softrains.intercom._

import com.lingeringsocket.shlurd.parser._

import scalaz._

class ContainsTopicMatcher(
  phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg,
  done : Boolean = false)
    extends TopicMatcher
{
  override def isDefinedAt(input : String) =
    ContainsTopicMatcher.matchPhrases(
      ContainsTopicMatcher.splitWords(input), phrases)

  override def apply(input : String) =
    (response, done)
}

object ContainsTopicMatcher
{
  def splitWords(input : String) : Array[String] =
    input.split(Array(' ', '.', ',', '(', ')', '?', '!', ';'))

  def matchPhrase(inputSplit : Array[String], phrase : String) =
    inputSplit.containsSlice(splitWords(phrase))

  def matchPhrases(inputSplit : Array[String], phrases : Seq[String]) =
    phrases.exists(phrase => matchPhrase(inputSplit, phrase))

  def string(
    phrases : Seq[String], response : => String) =
  {
    new ContainsTopicMatcher(
      phrases, IntercomActor.PartnerUtteranceMsg(response), false)
  }

  def string(
    phrases : Seq[String], response : => String, done : Boolean) =
  {
    new ContainsTopicMatcher(
      phrases, IntercomActor.PartnerUtteranceMsg(response), done)
  }

  def message(
    phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg) =
  {
    new ContainsTopicMatcher(phrases, response, false)
  }

  def message(
    phrases : Seq[String], response : => IntercomActor.SpeakerSoundMsg,
    done : Boolean) =
  {
    new ContainsTopicMatcher(phrases, response, done)
  }
}

class CatchAllTopicMatcher extends TopicMatcher
{
  private var first = true

  def clearFirst()
  {
    first = false
  }

  override def isDefinedAt(input : String) = true

  override def apply(input : String) =
  {
    if (input.isEmpty || first) {
      first = false
      (IntercomActor.PartnerUtteranceMsg("So, what is on your mind?"), false)
    } else {
      (IntercomActor.PartnerUtteranceMsg("I think you said, " + input), false)
    }
  }
}

class ShlurdTopicMatcher(persona : CommonPersona) extends TopicMatcher
{
  val sentenceMemo : String => SilSentence = Memo.immutableHashMapMemo {
    // FIXME:  deal with parseAll
    ShlurdParser(_).parseFirst
  }

  override def isDefinedAt(input : String) =
  {
    if (input.isEmpty || persona.getContext.getSettings.Test.active) {
      false
    } else {
      !sentenceMemo(input).hasUnknown
    }
  }

  override def apply(input : String) =
  {
    persona.shlurdRespond(sentenceMemo(input))
  }
}
