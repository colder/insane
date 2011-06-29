package insane
package predefined

import annotations._

@AbstractsClass("java.util.Set")
abstract class javautilSet {
  @AbstractsMethod("java.util.Set.add((x$1: java.lang.Object)Boolean)")
  def __add(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.clear(()Unit)")
  def __clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Set.contains((x$1: java.lang.Object)Boolean)")
  def __contains(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.iterator(()java.util.Iterator)")
  def __iterator(): java.util.Iterator[_]
  @AbstractsMethod("java.util.Set.remove((x$1: java.lang.Object)Boolean)")
  def __remove(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.size(()Int)")
  def __size(): Int = {
    42
  }
}
