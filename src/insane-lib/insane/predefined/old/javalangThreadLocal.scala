package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ThreadLocal")
abstract class javalangThreadLocal {
  @AbstractsMethod("java.lang.ThreadLocal.get(()java.lang.Object)")
  def __get(): java.lang.Object
  @AbstractsMethod("java.lang.ThreadLocal.<init>(()java.lang.ThreadLocal)")
  def ____init__(): javalangThreadLocal = {
    this
  }
  @AbstractsMethod("java.lang.ThreadLocal.set((x$1: java.lang.Object)Unit)")
  def __set(x1: java.lang.Object): Unit = {
    ()
  }
}
