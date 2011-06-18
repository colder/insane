package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ThreadLocal")
class javalangThreadLocal {
  @AbstractsMethod("java.lang.ThreadLocal.get(()java.lang.Object)")
  def get(): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.lang.ThreadLocal.<init>(()java.lang.ThreadLocal)")
  def __init__(): javalangThreadLocal = {
    this
  }
  @AbstractsMethod("java.lang.ThreadLocal.set((x$1: java.lang.Object)Unit)")
  def set(x1: java.lang.Object): Unit = {
    ()
  }
}
