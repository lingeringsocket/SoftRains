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

import softrains.conversation._

import akka.actor._

class CentralIntercom(
  name : String,
  actorRefFactory : ActorRefFactory,
  central : CentralService,
  intercomActorLookup : () => ActorRef)
{
  private val conversationActor =
    actorRefFactory.actorOf(
      Props(classOf[ConversationActor],
        central.getDatabase, central.getOntology),
      "conversationActor-" + name)

  def getConversationActor = conversationActor

  def getIntercomActor = intercomActorLookup()

  def getName = name
}
