package insane
package predefined

import annotations._

@AbstractsClass("java.lang.UnsupportedOperationException")
class javalangUnsupportedOperationException {
  @AbstractsMethod("java.lang.UnsupportedOperationException.<init>(()java.lang.UnsupportedOperationException)")
  def PLOPINIT() : java.lang.UnsupportedOperationException = { new java.lang.UnsupportedOperationException() }

  @AbstractsMethod("java.lang.UnsupportedOperationException.<init>((x$1:java.lang.String)java.lang.UnsupportedOperationException)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.UnsupportedOperationException = { new java.lang.UnsupportedOperationException() }

}
