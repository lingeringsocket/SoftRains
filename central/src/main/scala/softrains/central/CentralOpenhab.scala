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
package softrains.central

import akka.actor._

import softrains.base._

import org.joda.time._

import scala.collection._

import scala.util.parsing.json._

case class OpenhabItem(
  itemName : String,
  itemType : String,
  itemLabel : Option[String],
  groupNames : Seq[String])
{
}

object CentralOpenhab
{
  def parseItems(json : String) : Map[String, OpenhabItem] =
  {
    val tree = JSON.parseFull(json)
    tree match {
      case Some(list : List[Any]) => {
        val items = list.map(obj => {
          val map = obj.asInstanceOf[Map[String, Any]]
          val itemName = map("name").toString
          val itemType = map("type").toString
          val itemLabel = map.get("label").map(_.toString)
          val groupNames = map("groupNames") match {
            case list : Seq[_] => list.map(_.toString)
            case _ => Seq.empty
          }
          OpenhabItem(itemName, itemType, itemLabel, groupNames)
        })
        Map(items.map(item => (item.itemName, item)).toSeq:_*)
      }
      case _ => {
        throw new RuntimeException("Unexpected Openhab items JSON")
      }
    }
  }
}

class CentralOntology
{
  def getItems() : Map[String, OpenhabItem] = Map.empty

  def refresh() {}

  def getState(itemName : String) : Option[String] = None
}

class CentralOpenhabOntology(
  actorSystem : ActorSystem, settings : SoftRainsSettings)
    extends CentralOntology
{
  private val items = new mutable.LinkedHashMap[String, OpenhabItem]

  def newCentralOpenhab = new CentralOpenhab(actorSystem, settings)

  private def load()
  {
    items ++= newCentralOpenhab.readItems
  }

  override def refresh()
  {
    items.synchronized {
      items.clear
      load
    }
  }

  override def getItems() =
  {
    items.synchronized {
      if (items.isEmpty) {
        load
      }
      (new mutable.LinkedHashMap ++= items)
    }
  }

  override def getState(itemName : String) =
    newCentralOpenhab.getState(itemName)
}

class CentralOpenhab(actorSystem : ActorSystem, settings : SoftRainsSettings)
    extends HttpConsumer(actorSystem)
{
  import CentralOpenhab._

  private val results = new mutable.ArrayBuffer[String]

  private val now = readClockTime

  def readItems() : Map[String, OpenhabItem] =
  {
    val itemsUrl = settings.Openhab.url + "/rest/items"
    var result = Map[String, OpenhabItem]()
    fetchString(itemsUrl) {
      json => {
        result = parseItems(json)
      }
    }
    ensureSuccess
    result
  }

  def getState(itemName : String) : Option[String] =
  {
    if (unavailable) {
      return None
    }
    var fetchedState : Option[String] = None
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      itemName + "/state"
    fetchString(stateUrl) { state =>
      fetchedState = Some(state)
    }
    ensureSuccess
    fetchedState
  }

  def checkDoor(itemName : String, spokenName : String)
  {
    if (unavailable) {
      return
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      itemName + "/state"
    fetchString(stateUrl) { state =>
      if (state == "OPEN") {
        val timeUrl = settings.Openhab.url + "/rest/items/" +
          (itemName + "_time") + "/state"
        fetchString(timeUrl) { timeString =>
          val openSince = new DateTime(timeString)
          if (openSince.isBefore(now.minusMinutes(2))) {
            results.synchronized {
              results += "the " + spokenName +
                " has been open for more than two minutes."
            }
          }
        }
      }
    }
  }

  def retrieveResults() : String =
  {
    waitForCompletion
    if (!getFailure.isEmpty) {
      "I'm having trouble checking the state of the house."
    } else if (results.isEmpty) {
      ""
    } else {
      "Looks like " + results.mkString("  Also, ")
    }
  }

  private def unavailable =
    (settings.Openhab.url.isEmpty || settings.Test.active)

  def updateResidentNotificationFlag(resident : HomeResident, flag : Boolean)
  {
    if (unavailable) {
      return
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_message") + "/state"
    val state = if (flag) "ON" else "OFF"
    putString(stateUrl, state) {}
  }

  def updateResidentPhoneRadio(resident : HomeResident, state : String)
  {
    if (unavailable) {
      return
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_phone_radio") + "/state"
    putString(stateUrl, state) {}
  }

  def sendResidentNotification(resident : HomeResident, message : String)
  {
    if (unavailable) {
      return
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_phone_notifier") + "/state"
    putString(stateUrl, message) {}
  }

  def getResidentPrivacy(resident : HomeResident) : Boolean =
  {
    if (unavailable) {
      return false
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_privacy") + "/state"
    var result = true
    fetchString(stateUrl) { state =>
      if (state == "OFF") {
        result = false
      }
    }
    ensureSuccess
    result
  }

  def getResidentPresence(resident : HomeResident) : Boolean =
  {
    if (unavailable) {
      return false
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_presence") + "/state"
    var result = false
    fetchString(stateUrl) { state =>
      if (state == "ON") {
        result = true
      }
    }
    ensureSuccess
    result
  }
}
