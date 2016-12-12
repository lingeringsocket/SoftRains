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
package softrains.network

import xml._
import xml.parsing.NoBindingFactoryAdapter

class HTML5Parser extends NoBindingFactoryAdapter
{
  override def loadXML(source : InputSource, _p: SAXParser) =
  {
    loadXML(source)
  }

  def loadXML(source : InputSource) =
  {
    import nu.validator.htmlparser.{sax,common}
    import sax.HtmlParser
    import common.XmlViolationPolicy

    val reader = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(this)
    reader.parse(source)
    rootElem
  }
}

trait HtmlProcessor
{
  protected def getTdText(tbl : NodeSeq) : Seq[String] =
    (tbl \\ "td").map(_.text.trim)

  protected def getSpanText(nodeSeq : NodeSeq) : Seq[String] =
    (nodeSeq \\ "span").map(_.text.trim)

  protected def getInputValue(inputs : NodeSeq, name : String) : String =
  {
    val input = inputs.filter(input =>
      (input \ "@name").text.trim == name)
    (input \ "@value").text.trim
  }
}
