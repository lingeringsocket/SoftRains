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
package softrains.intercom

import softrains.base._

import akka.actor._

import scala.concurrent._

trait IntercomClient
{
  private var intercomActorLocal : Option[ActorRef] = None

  def getActorSystem : ActorSystem

  def getSettings : SoftRainsSettings

  def intercomActorTimeout =
    duration.FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS)

  private def isLocal(intercomSpec : String) = !intercomSpec.startsWith("akka:")

  protected def startLocalIntercoms()
  {
    getSettings.intercoms.foreach(intercom => {
      val intercomSpec = intercom.actor
      if (isLocal(intercomSpec) && intercomActorLocal.isEmpty) {
        val props = Props(classOf[IntercomActor])
        intercomActorLocal = Some(getActorSystem.actorOf(props, intercomSpec))
      }
    })
  }

  def accessIntercomActor(name : String) : ActorRef =
  {
    val intercom =
      getSettings.intercoms.find(_.name == name).get
    val intercomSpec = intercom.actor
    if (isLocal(intercomSpec) && !intercomActorLocal.isEmpty) {
      return intercomActorLocal.get
    }
    val intercomActorSelection = {
      if (isLocal(intercomSpec)) {
        getActorSystem.actorSelection(getActorSystem / intercomSpec)
      } else {
        getActorSystem.actorSelection(intercomSpec)
      }
    }
    val intercomActorFuture = intercomActorSelection.resolveOne(
      intercomActorTimeout)
    val result = Await.result(
      intercomActorFuture, intercomActorTimeout)
    if (isLocal(intercomSpec)) {
      intercomActorLocal = Some(result)
    }
    result
  }
}
