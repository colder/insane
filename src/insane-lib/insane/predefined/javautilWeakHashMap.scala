package insane
package predefined

import annotations._

@AbstractsClass("java.util.WeakHashMap")
abstract class javautilWeakHashMap {
  @AbstractsMethod("java.util.WeakHashMap.get((x$1: java.lang.Object)java.lang.Object)")
  def __get(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.WeakHashMap.<init>(()java.util.WeakHashMap)")
  def ____init__(): javautilWeakHashMap = {
    this
  }
  @AbstractsMethod("java.util.WeakHashMap.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def __put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
}
