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

class PersonaWorld(openhab : CentralOpenhab) extends ShlurdWorld
{
  override def resolveReference(
    reference : ShlurdReference,
    context : ShlurdReferenceContext) =
  {
    reference match {
      case ShlurdEntityReference(entity, determiner, count) => {
        // FIXME cache, and derive lemmas from names+labels+tags
        val items = openhab.readItems
        items.get(entity.lemma) match {
          case Some(item) => Success(ItemEntity(item.itemName))
          case _ => fail("I don't know about this named entity: " + entity.lemma)
        }
      }
      case _ => fail("I don't know about this entity reference: " + reference)
    }
    fail("Huh?")
  }

  override def resolveProperty(
    entity : ShlurdEntity,
    lemma : String) =
  {
    fail("Huh?")
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdEntity,
    property : ShlurdProperty,
    lemma : String) =
  {
    fail("Huh?")
  }

  override def evaluateEntityLocationPredicate(
    entity : ShlurdEntity,
    location : ShlurdEntity,
    locative : ShlurdLocative) =
  {
    fail("Huh?")
  }
}
