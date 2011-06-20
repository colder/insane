package insane
package predefined

import annotations._

@AbstractsClass("java.util.AbstractMap")
abstract class javautilAbstractMap {
  @AbstractsMethod("java.util.AbstractMap.<init>(()java.util.AbstractMap)")
  def __init__(): javautilAbstractMap = {
    this
  }
  @AbstractsMethod("java.util.AbstractMap.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
}
