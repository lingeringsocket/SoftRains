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

import akka.actor._
import com.typesafe.config._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

import java.io._
import java.util.concurrent._

class SoftRainsSettings(rootConf : Config)
{
  private val conf = rootConf.getConfig("softrains")

  private def getMillis(subConf: Config, path : String) =
    FiniteDuration(
      subConf.getDuration(path, TimeUnit.MILLISECONDS), MILLISECONDS)

  object Http
  {
    val subConf = conf.getConfig("http")
    val address = subConf.getString("address")
    val port = subConf.getInt("port")
  }

  object Actors
  {
    val subConf = conf.getConfig("actors")
    val central = subConf.getString("central")
    val intercom = subConf.getString("intercom")
    val echo = subConf.getString("echo")
    val camera = subConf.getString("camera")
  }

  object Openhab
  {
    val subConf = conf.getConfig("openhab")
    val url = subConf.getString("url")
  }

  object Router
  {
    val subConf = conf.getConfig("router")
    val url = subConf.getString("url")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
    val scanFreq = getMillis(subConf, "scanFreq")
  }

  object Db
  {
    val subConf = conf.getConfig("db")
    val url = subConf.getString("url")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
  }

  object Mail
  {
    val subConf = conf.getConfig("mail")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
    val enabled = subConf.getBoolean("enabled")
  }

  object WatsonTts
  {
    val subConf = conf.getConfig("watson-tts")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
  }

  object WatsonStt
  {
    val subConf = conf.getConfig("watson-stt")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
  }

  object Alexa
  {
    val subConf = conf.getConfig("alexa")
    val confFile = new File(subConf.getString("conf-file")).getAbsoluteFile
  }

  object Speaker
  {
    val subConf = conf.getConfig("speaker")
    val command = subConf.getString("command")
    val playFileCommand = subConf.getString("play-file-command")
    val loopFileCommand = subConf.getString("loop-file-command")
    val doorbellCommand = subConf.getString("doorbell-command")
    val ringtoneCommand = subConf.getString("ringtone-command")
    val sleepCommand = subConf.getString("sleep-command")
    val wakeCommand = subConf.getString("wake-command")
    val sleepTimeout = getMillis(subConf, "sleep-timeout")
    val soundPath = new File(subConf.getString("sound-path")).getAbsoluteFile
  }

  object Files
  {
    val subConf = conf.getConfig("files")
    val videoPath = new File(subConf.getString("video-path")).getAbsoluteFile
    val audioPath = new File(subConf.getString("audio-path")).getAbsoluteFile
  }

  object Visitors
  {
    val subConf = conf.getConfig("visitors")
    val trainingPath =
      new File(subConf.getString("training-path")).getAbsoluteFile
    val frameInterval = getMillis(subConf, "frame-interval")
    val blobMergeDistance = subConf.getDouble("blob-merge-distance")
    val blobMinSize = subConf.getDouble("blob-min-size")
    val bodyMinSize = subConf.getDouble("body-min-size")
    val proximityZone = subConf.getDouble("proximity-zone")
  }

  object Test
  {
    val subConf = conf.getConfig("test")
    val active = subConf.getBoolean("active")
  }

  object Residents
  {
    val subConf = conf.getConfig("residents")
    val deviceMap = readMap(subConf, "devices")
    val emailMap = readMap(subConf, "email")
  }

  object Cameras
  {
    val subConf = conf.getConfig("cameras")
    val urlMap = readMap(subConf, "urls")
  }

  object Kiosk
  {
    val subConf = conf.getConfig("kiosk")
    val cameraUrl = subConf.getString("camera-url")
  }

  private def readMap(conf : Config, key : String) : Map[String, String] =
  {
    val list = conf.getObjectList(key).asScala
    Map(list.flatMap(_.entrySet.asScala).toSeq.map(entry => (
      entry.getKey, entry.getValue.unwrapped.toString)):_*)
  }
}

object SoftRainsSettings
{
  def apply(config : Config) = new SoftRainsSettings(config)

  def complainMissing(path : String)
  {
    throw new ConfigException.Missing(path)
  }
}

class SoftRainsActorSettings(
  rootConf : Config, extendedSystem : ExtendedActorSystem)
    extends SoftRainsSettings(rootConf)
    with Extension
{
}

object SoftRainsActorSettings
    extends ExtensionId[SoftRainsActorSettings] with ExtensionIdProvider
{
  override def lookup = SoftRainsActorSettings

  override def createExtension(system : ExtendedActorSystem) =
    new SoftRainsActorSettings(system.settings.config, system)

  def apply(context : ActorContext) : SoftRainsActorSettings =
    apply(context.system)
}
