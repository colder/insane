package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Runtime")
class javalangRuntime {
  @AbstractsMethod("java.lang.Runtime.addShutdownHook((x$1:java.lang.Thread)Unit)")
  def addShutdownHook(x1 : java.lang.Thread) : Unit = { () }

  @AbstractsMethod("java.lang.Runtime.availableProcessors(()Int)")
  def availableProcessors() : Int = { 0 }

  @AbstractsMethod("java.lang.RuntimeException.<init>(()java.lang.RuntimeException)")
  def PLOPINIT() : java.lang.RuntimeException = { new java.lang.RuntimeException() }

  @AbstractsMethod("java.lang.RuntimeException.<init>((x$1:java.lang.String)java.lang.RuntimeException)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.RuntimeException = { new java.lang.RuntimeException() }

  @AbstractsMethod("java.lang.Runtime.getRuntime(()java.lang.Runtime)")
  def getRuntime() : java.lang.Runtime = { new java.lang.Runtime() }

  @AbstractsMethod("java.lang.Runtime.removeShutdownHook((x$1:java.lang.Thread)Boolean)")
  def removeShutdownHook(x1 : java.lang.Thread) : Boolean = { true }

}
