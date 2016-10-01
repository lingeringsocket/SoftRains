package softrains

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
