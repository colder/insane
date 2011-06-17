package insane
package predefined

import annotations._

@AbstractsClass("org.xml.sax.Attributes")
class orgxmlsaxAttributes {
  @AbstractsMethod("org.xml.sax.Attributes.getLength(()Int)")
  def getLength() : Int = { 0 }

  @AbstractsMethod("org.xml.sax.Attributes.getQName((x$1:Int)java.lang.String)")
  def getQName(x1 : Int) : java.lang.String = { "" }

  @AbstractsMethod("org.xml.sax.Attributes.getValue((x$1:Int)java.lang.String)")
  def getValue(x1 : Int) : java.lang.String = { "" }

  @AbstractsMethod("org.xml.sax.Attributes.getValue((x$1:java.lang.String)java.lang.String)")
  def getValue(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("org.xml.sax.Attributes.getValue((x$1:java.lang.String, x$2:java.lang.String)java.lang.String)")
  def getValue(x1 : java.lang.String, x2 : java.lang.String) : java.lang.String = { "" }

}
