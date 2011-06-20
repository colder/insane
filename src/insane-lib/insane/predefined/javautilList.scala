package insane
package predefined

import annotations._

@AbstractsClass("java.util.List")
abstract class javautilList {
  @AbstractsMethod("java.util.List.add((x$1: java.lang.Object)Boolean)")
  def add(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.List.clear(()Unit)")
  def clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.List.get((x$1: Int)java.lang.Object)")
  def get(x1: Int): java.lang.Object
  @AbstractsMethod("java.util.List.isEmpty(()Boolean)")
  def isEmpty(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.List.iterator(()java.util.Iterator)")
  def iterator(): java.util.Iterator[_]
  @AbstractsMethod("java.util.List.remove((x$1: Int)java.lang.Object)")
  def remove(x1: Int): java.lang.Object
  @AbstractsMethod("java.util.List.set((x$1: Int, x$2: java.lang.Object)java.lang.Object)")
  def set(x1: Int, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.List.size(()Int)")
  def size(): Int = {
    42
  }
  @AbstractsMethod("java.util.List.subList((x$1: Int, x$2: Int)java.util.List)")
  def subList(x1: Int, x2: Int): java.util.List[_]
}
