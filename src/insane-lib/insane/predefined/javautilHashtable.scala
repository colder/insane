package insane
package predefined

import annotations._

@AbstractsClass("java.util.Hashtable")
abstract class javautilHashtable {
  @AbstractsMethod("java.util.Hashtable.clear(()Unit)")
  def clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Hashtable.entrySet(()java.util.Set)")
  def entrySet(): java.util.Set[_]
  @AbstractsMethod("java.util.Hashtable.get((x$1: java.lang.Object)java.lang.Object)")
  def get(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Hashtable.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Hashtable.remove((x$1: java.lang.Object)java.lang.Object)")
  def remove(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Hashtable.size(()Int)")
  def size(): Int = {
    42
  }
}
