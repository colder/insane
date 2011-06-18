package insane
package predefined

import annotations._

@AbstractsClass("javax.xml.parsers.SAXParserFactory")
class javaxxmlparsersSAXParserFactory {
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newInstance(()javax.xml.parsers.SAXParserFactory)")
  def newInstance(): javax.xml.parsers.SAXParserFactory = {
    new javax.xml.parsers.SAXParserFactory()
  }
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newSAXParser(()javax.xml.parsers.SAXParser)")
  def newSAXParser(): javax.xml.parsers.SAXParser = {
    new javax.xml.parsers.SAXParser()
  }
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.setNamespaceAware((x$1: Boolean)Unit)")
  def setNamespaceAware(x1: Boolean): Unit = {
    ()
  }
}
