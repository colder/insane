package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Short")
class javalangShort {
  @AbstractsMethod("java.lang.Short.parseShort((x$1: java.lang.String)Short)")
  def parseShort(x1: java.lang.String): Short = {
    (42 : Short)
  }
  @AbstractsMethod("java.lang.Short.shortValue(()Short)")
  def shortValue(): Short = {
    (42 : Short)
  }
  @AbstractsMethod("java.lang.Short.valueOf((x$1: Short)java.lang.Short)")
  def valueOf(x1: Short): java.lang.Short = {
    new java.lang.Short(42 : Short)
  }
}
