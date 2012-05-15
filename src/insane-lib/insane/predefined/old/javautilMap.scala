package insane
package predefined

import annotations._

@AbstractsClass("java.util.Map")
abstract class javautilMap {
  @AbstractsMethod("java.util.Map.clear(()Unit)")
  def __clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Map.containsKey((x$1: java.lang.Object)Boolean)")
  def __containsKey(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Map.entrySet(()java.util.Set)")
  def __entrySet(): java.util.Set[_]
  @AbstractsMethod("java.util.Map.get((x$1: java.lang.Object)java.lang.Object)")
  def __get(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Map.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def __put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Map.remove((x$1: java.lang.Object)java.lang.Object)")
  def __remove(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Map.size(()Int)")
  def __size(): Int = {
    42
  }
}
