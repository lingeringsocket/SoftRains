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
package softrains.base

import com.typesafe.config._

import akka.actor._
import akka.testkit._

import org.specs2.mutable._

import java.util.concurrent.atomic._

import scala.concurrent.duration._

object AkkaActorSpecification
{
  private val suffixGenerator = new AtomicLong
}

abstract class AkkaActorSpecification(confFile : String = "test.conf")
    extends Specification
{
  protected val settings = SoftRainsSettings(loadConfig(confFile))

  protected def configureSystem(overrideConf : String) =
    ActorSystem(
      "TestActors_" + AkkaActorSpecification.suffixGenerator.incrementAndGet,
      loadConfig(overrideConf))

  protected def loadConfig(overrideConf : String) =
  {
    val actualConf = {
      if (overrideConf.isEmpty) {
        confFile
      } else {
        overrideConf
      }
    }
    if (actualConf.isEmpty) {
      ConfigFactory.load
    } else {
      ConfigFactory.load(actualConf)
    }
  }

  abstract class AkkaActorExample(overrideConf : String = "test.conf")
      extends TestKit(configureSystem(overrideConf))
      with After
      with ImplicitSender
  {
    def after =
      TestKit.shutdownActorSystem(system, Duration.Inf, true)
  }
}
