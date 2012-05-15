package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Runtime")
abstract class javalangRuntime {
  @AbstractsMethod("java.lang.Runtime.addShutdownHook((x$1: java.lang.Thread)Unit)")
  def __addShutdownHook(x1: java.lang.Thread): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Runtime.availableProcessors(()Int)")
  def __availableProcessors(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Runtime.getRuntime(()java.lang.Runtime)")
  def __getRuntime(): java.lang.Runtime
  @AbstractsMethod("java.lang.Runtime.removeShutdownHook((x$1: java.lang.Thread)Boolean)")
  def __removeShutdownHook(x1: java.lang.Thread): Boolean = {
    true
  }
}
