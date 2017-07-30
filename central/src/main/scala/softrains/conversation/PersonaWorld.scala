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

import shlurd.parser._
import shlurd.world._

import scala.util._

case class ItemEntity(itemName : String) extends ShlurdEntity
{
}

class PersonaWorld(ontology : CentralOntology)
    extends ShlurdWorld[ItemEntity, ShlurdProperty]
{
  override def resolveUnqualifiedEntity(
    lemma : String,
    context : ShlurdReferenceContext) =
  {
    // FIXME:  something real
    val items = ontology.getItems
    items.get(lemma) match {
      case Some(item) => Success(Set(ItemEntity(item.itemName)))
      case _ => fail(
        "I don't know about this named entity: " + lemma)
    }
  }

  override def resolveProperty(
    entity : ItemEntity,
    lemma : String) =
  {
    fail("Huh?")
  }

  override def evaluateEntityPropertyPredicate(
    entity : ItemEntity,
    property : ShlurdProperty,
    lemma : String) =
  {
    fail("Huh?")
  }

  override def evaluateEntityLocationPredicate(
    entity : ItemEntity,
    location : ItemEntity,
    locative : ShlurdLocative) =
  {
    fail("Huh?")
  }
}
