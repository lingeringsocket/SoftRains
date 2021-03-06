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
package softrains.kiosk

import softrains.base._

import akka.actor._

import scala.concurrent._
import scala.io._

import com.typesafe.config._

object KioskApp extends App
{
  private val config = ConfigFactory.load
  private val settings = SoftRainsSettings(config)
  private val system = ActorSystem("SoftRainsKiosk", config)
  private val kioskSpec = settings.Actors.kiosk
  assert (!kioskSpec.isEmpty)
  private val kioskActor =
    system.actorOf(Props(classOf[KioskActor], None, true), kioskSpec)

  println("Akka listening, press RETURN to stop...")
  StdIn.readLine
  kioskActor ! PoisonPill

  system.terminate
  Await.result(system.whenTerminated, duration.Duration.Inf)
}
