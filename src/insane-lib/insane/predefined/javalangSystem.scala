package insane
package predefined

import annotations._

@AbstractsClass("java.lang.System")
abstract class javalangSystem {
  @AbstractsMethod("java.lang.System.arraycopy((x$1: java.lang.Object, x$2: Int, x$3: java.lang.Object, x$4: Int, x$5: Int)Unit)")
  def __arraycopy(x1: java.lang.Object, x2: Int, x3: java.lang.Object, x4: Int, x5: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.System.clearProperty((x$1: java.lang.String)java.lang.String)")
  def __clearProperty(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.System.currentTimeMillis(()Long)")
  def __currentTimeMillis(): Long = {
    42L
  }
  @AbstractsMethod("java.lang.System.exit((x$1: Int)Unit)")
  def __exit(x1: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.System.gc(()Unit)")
  def __gc(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.System.getenv(()java.util.Map)")
  def __getenv(): java.util.Map[_,_]
  @AbstractsMethod("java.lang.System.getenv((x$1: java.lang.String)java.lang.String)")
  def __getenv(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.System.getProperties(()java.util.Properties)")
  def __getProperties(): java.util.Properties
  @AbstractsMethod("java.lang.System.getProperty((x$1: java.lang.String)java.lang.String)")
  def __getProperty(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.System.getProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.String)")
  def __getProperty(x1: java.lang.String, x2: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.System.identityHashCode((x$1: java.lang.Object)Int)")
  def __identityHashCode(x1: java.lang.Object): Int = {
    42
  }
  @AbstractsMethod("java.lang.System.setProperty((x$1: java.lang.String, x$2: java.lang.String)java.lang.String)")
  def __setProperty(x1: java.lang.String, x2: java.lang.String): java.lang.String = {
    ""
  }
}
