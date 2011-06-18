package insane
package predefined

import annotations._

@AbstractsClass("java.lang.InheritableThreadLocal")
class javalangInheritableThreadLocal {
  @AbstractsMethod("java.lang.InheritableThreadLocal.<init>(()java.lang.InheritableThreadLocal)")
  def __init__(): java.lang.InheritableThreadLocal = {
    new java.lang.InheritableThreadLocal()
  }
}
