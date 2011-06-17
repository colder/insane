package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Exception")
class javalangException {
  @AbstractsMethod("java.lang.Exception.<init>(()java.lang.Exception)")
  def PLOPINIT() : java.lang.Exception = { new java.lang.Exception() }

  @AbstractsMethod("java.lang.Exception.<init>((x$1:java.lang.String)java.lang.Exception)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.Exception = { new java.lang.Exception() }

}
