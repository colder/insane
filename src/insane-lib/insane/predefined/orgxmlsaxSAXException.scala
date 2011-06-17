package insane
package predefined

import annotations._

@AbstractsClass("org.xml.sax.SAXException")
class orgxmlsaxSAXException {
  @AbstractsMethod("org.xml.sax.SAXException.getMessage(()java.lang.String)")
  def getMessage() : java.lang.String = { "" }

  @AbstractsMethod("org.xml.sax.SAXException.<init>((x$1:java.lang.String)org.xml.sax.SAXException)")
  def PLOPINIT(x1 : java.lang.String) : org.xml.sax.SAXException = { new org.xml.sax.SAXException() }

  @AbstractsMethod("org.xml.sax.SAXException.<init>((x$1:java.lang.String, x$2:java.lang.Exception)org.xml.sax.SAXException)")
  def PLOPINIT(x1 : java.lang.String, x2 : java.lang.Exception) : org.xml.sax.SAXException = { new org.xml.sax.SAXException() }

}
