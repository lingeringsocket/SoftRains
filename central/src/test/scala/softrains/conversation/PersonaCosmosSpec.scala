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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.platonic._

import softrains.central._

import org.specs2.mutable._
import org.specs2.specification._

import scala.io._

class PersonaCosmosSpec extends Specification
{
  val items = CentralOpenhab.parseItems(
    ResourceUtils.readResource("/items.json")).toMap

  val itemStates = Map(
    "Window_GF_Kitchen" -> "open"
  )

  val ontology = new CentralOntology {
    override def getItems() = items

    override def getState(itemName : String) = itemStates.get(itemName)
  }

  trait CosmosContext extends Scope
  {
    val cosmos = new PersonaCosmos(ontology)
    val mind = new SpcMind(cosmos)
    mind.loadBeliefs(Source.fromFile(
      ResourceUtils.getResourceFile("/beliefs.txt")))
    cosmos.loadItems

    val responder = new SpcResponder(mind)

    protected def process(input : String, expected : String) =
    {
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence, input) must be equalTo(expected)
    }
  }

  "PersonaCosmos" should
  {
    "understand static structure" in new CosmosContext
    {
      skipped("borked")
      process(
        "is there a bathroom on the first floor",
        "Yes, there is a bathroom on the first floor.")
    }

    "understand dynamic state" in new CosmosContext
    {
      process(
        "is any window open",
        "Yes, the kitchen window is open.")
    }

    "understand unknown state" in new CosmosContext
    {
      skipped("borked")
      process(
        "is the toilet window open",
        "I don't know.")
    }
  }
}
