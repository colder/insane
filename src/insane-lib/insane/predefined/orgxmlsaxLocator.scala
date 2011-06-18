package insane
package predefined

import annotations._

@AbstractsClass("org.xml.sax.Locator")
class orgxmlsaxLocator {
  @AbstractsMethod("org.xml.sax.Locator.getColumnNumber(()Int)")
  def getColumnNumber(): Int = {
    42
  }
  @AbstractsMethod("org.xml.sax.Locator.getLineNumber(()Int)")
  def getLineNumber(): Int = {
    42
  }
  @AbstractsMethod("org.xml.sax.Locator.getPublicId(()java.lang.String)")
  def getPublicId(): java.lang.String = {
    ""
  }
  @AbstractsMethod("org.xml.sax.Locator.getSystemId(()java.lang.String)")
  def getSystemId(): java.lang.String = {
    ""
  }
}
