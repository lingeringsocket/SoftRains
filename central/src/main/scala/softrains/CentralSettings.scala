package softrains

import com.typesafe.config._
import scala.collection.JavaConverters._

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
