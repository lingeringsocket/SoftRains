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
package softrains.central

import akka.actor._

import softrains.base._

import org.joda.time._

import scala.collection._

class CentralOpenhab(actorSystem : ActorSystem, settings : SoftRainsSettings)
    extends HttpConsumer(actorSystem)
{
  private val results = new mutable.ArrayBuffer[String]

  // FIXME share readClockTime
  private val now = new DateTime(DateTimeZone.UTC)

  def checkDoor(itemName : String, spokenName : String)
  {
    if (settings.Openhab.url.isEmpty) {
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

  def updateResidentPhoneRadio(resident : HomeResident, state : String)
  {
    if (settings.Openhab.url.isEmpty || settings.Test.active) {
      return
    }
    val stateUrl = settings.Openhab.url + "/rest/items/" +
      (resident.name.toLowerCase + "_phone_radio") + "/state"
    putString(stateUrl, state) {}
  }
}
