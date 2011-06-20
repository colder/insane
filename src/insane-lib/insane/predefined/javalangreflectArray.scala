package insane
package predefined

import annotations._

@AbstractsClass("java.lang.reflect.Array")
abstract class javalangreflectArray {
  @AbstractsMethod("java.lang.reflect.Array.newInstance((x$1: java.lang.Class, x$2: Int)java.lang.Object)")
  def newInstance(x1: java.lang.Class, x2: Int): java.lang.Object
}
