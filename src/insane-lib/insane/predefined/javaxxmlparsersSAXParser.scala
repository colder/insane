package insane
package predefined

import annotations._

@AbstractsClass("javax.xml.parsers.SAXParser")
class javaxxmlparsersSAXParser {
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newInstance(()javax.xml.parsers.SAXParserFactory)")
  def newInstance() : javax.xml.parsers.SAXParserFactory = { new javax.xml.parsers.SAXParserFactory() }

  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newSAXParser(()javax.xml.parsers.SAXParser)")
  def newSAXParser() : javax.xml.parsers.SAXParser = { new javax.xml.parsers.SAXParser() }

  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.setNamespaceAware((x$1:Boolean)Unit)")
  def setNamespaceAware(x1 : Boolean) : Unit = { () }

  @AbstractsMethod("javax.xml.parsers.SAXParser.parse((x$1:org.xml.sax.InputSource, x$2:org.xml.sax.helpers.DefaultHandler)Unit)")
  def parse(x1 : org.xml.sax.InputSource, x2 : org.xml.sax.helpers.DefaultHandler) : Unit = { () }

}
