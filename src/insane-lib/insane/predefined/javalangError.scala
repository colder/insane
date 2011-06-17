package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Error")
class javalangError {
  @AbstractsMethod("java.lang.Error.<init>(()java.lang.Error)")
  def PLOPINIT() : java.lang.Error = { new java.lang.Error() }

  @AbstractsMethod("java.lang.Error.<init>((x$1:java.lang.String)java.lang.Error)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.Error = { new java.lang.Error() }

}
