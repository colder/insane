package insane
package predefined

import annotations._

@AbstractsClass("java.lang.IllegalArgumentException")
class javalangIllegalArgumentException {
  @AbstractsMethod("java.lang.IllegalArgumentException.<init>(()java.lang.IllegalArgumentException)")
  def PLOPINIT() : java.lang.IllegalArgumentException = { new java.lang.IllegalArgumentException() }

  @AbstractsMethod("java.lang.IllegalArgumentException.<init>((x$1:java.lang.String)java.lang.IllegalArgumentException)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.IllegalArgumentException = { new java.lang.IllegalArgumentException() }

}
