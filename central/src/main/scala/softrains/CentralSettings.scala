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
package softrains

import com.typesafe.config._

import scala.collection.JavaConverters._

import java.io._

class CentralSettings(rootConf : Config)
{
  private val conf = rootConf.getConfig("softrains")

  object Router
  {
    val subConf = conf.getConfig("router")
    val url = subConf.getString("url")
    val user = subConf.getString("user")
    val password = subConf.getString("password")
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
  }

  object Files
  {
    val subConf = conf.getConfig("files")
    val videoPath = new File(subConf.getString("video-path"))
  }

  object Visitors
  {
    val subConf = conf.getConfig("visitors")
    val minSize = subConf.getDouble("min-size")
    val proximityZone = subConf.getDouble("proximity-zone")
    val mergeDistance = subConf.getDouble("merge-distance")
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

  private def readMap(conf : Config, key : String) : Map[String, String] =
  {
    val list = conf.getObjectList(key).asScala
    Map(list.flatMap(_.entrySet.asScala).toSeq.map(entry => (
      entry.getKey, entry.getValue.unwrapped.toString)):_*)
  }
}

object CentralSettings
{
  def apply(config : Config) = new CentralSettings(config)

  def complainMissing(path : String)
  {
    throw new ConfigException.Missing(path)
  }
}
