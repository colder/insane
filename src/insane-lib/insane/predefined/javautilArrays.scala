package insane
package predefined

import annotations._

@AbstractsClass("java.util.Arrays")
class javautilArrays {
  @AbstractsMethod("java.util.Arrays.fill((x$1: Array[Int], x$2: Int)Unit)")
  def __fill(x1: Array[Int], x2: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Arrays.sort((x$1: Array[java.lang.Object], x$2: java.util.Comparator)Unit)")
  def __sort(x1: Array[java.lang.Object], x2: java.util.Comparator[_]): Unit = {
    ()
  }
}
