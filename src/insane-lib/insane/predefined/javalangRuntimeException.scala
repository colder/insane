package insane
package predefined

import annotations._

@AbstractsClass("java.lang.RuntimeException")
class javalangRuntimeException {
  @AbstractsMethod("java.lang.RuntimeException.<init>(()java.lang.RuntimeException)")
  def PLOPINIT() : java.lang.RuntimeException = { new java.lang.RuntimeException() }

  @AbstractsMethod("java.lang.RuntimeException.<init>((x$1:java.lang.String)java.lang.RuntimeException)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.RuntimeException = { new java.lang.RuntimeException() }

}
