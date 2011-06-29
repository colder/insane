package insane
package predefined

import annotations._

@AbstractsClass("javax.xml.parsers.SAXParserFactory")
abstract class javaxxmlparsersSAXParserFactory {
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newInstance(()javax.xml.parsers.SAXParserFactory)")
  def __newInstance(): javax.xml.parsers.SAXParserFactory
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.newSAXParser(()javax.xml.parsers.SAXParser)")
  def __newSAXParser(): javax.xml.parsers.SAXParser
  @AbstractsMethod("javax.xml.parsers.SAXParserFactory.setNamespaceAware((x$1: Boolean)Unit)")
  def __setNamespaceAware(x1: Boolean): Unit = {
    ()
  }
}
