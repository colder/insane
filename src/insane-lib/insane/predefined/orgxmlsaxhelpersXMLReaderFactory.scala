package insane
package predefined

import annotations._

@AbstractsClass("org.xml.sax.helpers.XMLReaderFactory")
class orgxmlsaxhelpersXMLReaderFactory {
  @AbstractsMethod("org.xml.sax.helpers.XMLReaderFactory.createXMLReader(()org.xml.sax.XMLReader)")
  def createXMLReader(): org.xml.sax.XMLReader = {
    new org.xml.sax.XMLReader()
  }
  @AbstractsMethod("org.xml.sax.helpers.XMLReaderFactory.createXMLReader((x$1: java.lang.String)org.xml.sax.XMLReader)")
  def createXMLReader(x1: java.lang.String): org.xml.sax.XMLReader = {
    new org.xml.sax.XMLReader()
  }
}
