package insane
package predefined

import annotations._

@AbstractsClass("java.util.Set")
class javautilSet {
  @AbstractsMethod("java.util.Set.add((x$1: java.lang.Object)Boolean)")
  def add(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.clear(()Unit)")
  def clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Set.contains((x$1: java.lang.Object)Boolean)")
  def contains(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.iterator(()java.util.Iterator)")
  def iterator(): java.util.Iterator = {
    new java.util.Iterator()
  }
  @AbstractsMethod("java.util.Set.remove((x$1: java.lang.Object)Boolean)")
  def remove(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Set.size(()Int)")
  def size(): Int = {
    42
  }
}
