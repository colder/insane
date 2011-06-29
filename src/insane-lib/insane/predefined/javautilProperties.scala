package insane
package predefined

import annotations._

@AbstractsClass("java.util.Properties")
abstract class javautilProperties {
  @AbstractsMethod("java.util.Properties.getProperty((x$1: java.lang.String)java.lang.String)")
  def __getProperty(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.Properties.getProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.String)")
  def __getProperty(x1: java.lang.String, x2: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.Properties.<init>(()java.util.Properties)")
  def ____init__(): javautilProperties = {
    this
  }
  @AbstractsMethod("java.util.Properties.load((x$1: java.io.InputStream)Unit)")
  def __load(x1: java.io.InputStream): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Properties.setProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.Object)")
  def __setProperty(x1: java.lang.String, x2: java.lang.String): java.lang.Object
}
