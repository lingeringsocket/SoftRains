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

import dispatch._, Defaults._

case class DeviceState(
  name : String,
  displayName : String,
  connectionType : String,
  ipAddress : String,
  macAddress : String
)

trait DeviceMonitor
{
  def loginIfNeeded()

  def requestLogin()

  def scanDevices() : Iterable[DeviceState]
}

abstract class AbstractCableRouterMonitor
    extends DeviceMonitor with HtmlProcessor
{
  protected def fetchDevices() : String

  override def scanDevices() =
  {
    val html = fetchDevices
    val parser = new HTML5Parser
    val xml = parser.loadString(html)
    val validationDivs = (xml \\ "div").filter(
      div => (div \ "@class").text.trim == "cnt-device-main")
    if (validationDivs.isEmpty) {
      throw new RuntimeException("Unexpected device HTML")
    }
    val forms = (xml \\ "form").filter(
      form => getSpanText(form).contains("Host Name:"))
    forms.map(form => {
      val spanText = getSpanText(form)
      val name = spanText(1)
      val connectionType = spanText(3)
      val inputs = form \\ "input"
      val ipAddress = getInputValue(inputs, "staticIPAddress")
      val macAddress = getInputValue(inputs, "mac_address")
      DeviceState(name + " " + macAddress, name, connectionType, ipAddress,
        macAddress)
    })
  }
}

class CableRouterMonitor(settings : CentralSettings)
    extends AbstractCableRouterMonitor
{
  private var cookies = ""

  override protected def fetchDevices() =
  {
    val request = (url(settings.Router.url) / "connected_devices_computers.php").
      addHeader("Cookie", cookies)
    val result = Http(request OK as.String)
    result()
  }

  override def loginIfNeeded()
  {
    if (!cookies.isEmpty) {
      return
    }
    val request = (url(settings.Router.url) / "check.php") << Map(
      "username" -> settings.Router.user,
      "password" -> settings.Router.password)
    val result = Http(request)
    cookies = result().getHeader("Set-Cookie")
  }

  override def requestLogin()
  {
    cookies = ""
  }
}
