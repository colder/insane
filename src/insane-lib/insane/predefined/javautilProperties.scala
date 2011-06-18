package insane
package predefined

import annotations._

@AbstractsClass("java.util.Properties")
class javautilProperties {
  @AbstractsMethod("java.util.Properties.getProperty((x$1: java.lang.String)java.lang.String)")
  def getProperty(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.Properties.getProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.String)")
  def getProperty(x1: java.lang.String, x2: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.Properties.<init>(()java.util.Properties)")
  def __init__(): java.util.Properties = {
    new java.util.Properties()
  }
  @AbstractsMethod("java.util.Properties.load((x$1: java.io.InputStream)Unit)")
  def load(x1: java.io.InputStream): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Properties.setProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.Object)")
  def setProperty(x1: java.lang.String, x2: java.lang.String): java.lang.Object = {
    new java.lang.Object()
  }
}
