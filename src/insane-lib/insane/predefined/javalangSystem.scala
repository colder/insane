package insane
package predefined

import annotations._

@AbstractsClass("java.lang.System")
class javalangSystem {
  @AbstractsMethod("java.lang.System.arraycopy((x$1:java.lang.Object, x$2:Int, x$3:java.lang.Object, x$4:Int, x$5:Int)Unit)")
  def arraycopy(x1 : java.lang.Object, x2 : Int, x3 : java.lang.Object, x4 : Int, x5 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.System.clearProperty((x$1:java.lang.String)java.lang.String)")
  def clearProperty(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.System.currentTimeMillis(()Long)")
  def currentTimeMillis() : Long = { 0L }

  @AbstractsMethod("java.lang.System.exit((x$1:Int)Unit)")
  def exit(x1 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.System.gc(()Unit)")
  def gc() : Unit = { () }

  @AbstractsMethod("java.lang.System.getenv(()java.util.Map)")
  def getenv() : java.util.Map = { new java.util.Map() }

  @AbstractsMethod("java.lang.System.getenv((x$1:java.lang.String)java.lang.String)")
  def getenv(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.System.getProperties(()java.util.Properties)")
  def getProperties() : java.util.Properties = { new java.util.Properties() }

  @AbstractsMethod("java.lang.System.getProperty((x$1:java.lang.String)java.lang.String)")
  def getProperty(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.System.getProperty((x$1:java.lang.String, x$2:java.lang.String)java.lang.String)")
  def getProperty(x1 : java.lang.String, x2 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.System.identityHashCode((x$1:java.lang.Object)Int)")
  def identityHashCode(x1 : java.lang.Object) : Int = { 0 }

  @AbstractsMethod("java.lang.System.setProperty((x$1:java.lang.String, x$2:java.lang.String)java.lang.String)")
  def setProperty(x1 : java.lang.String, x2 : java.lang.String) : java.lang.String = { "" }

}
