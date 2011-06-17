package insane
package predefined

import annotations._

@AbstractsClass("java.util.Map")
class javautilMap {
  @AbstractsMethod("java.util.Map.clear(()Unit)")
  def clear() : Unit = { () }

  @AbstractsMethod("java.util.Map.containsKey((x$1:java.lang.Object)Boolean)")
  def containsKey(x1 : java.lang.Object) : Boolean = { true }

  @AbstractsMethod("java.util.Map$Entry.getKey(()java.lang.Object)")
  def getKey() : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map$Entry.getValue(()java.lang.Object)")
  def getValue() : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map.entrySet(()java.util.Set)")
  def entrySet() : java.util.Set = { new java.util.Set() }

  @AbstractsMethod("java.util.Map.get((x$1:java.lang.Object)java.lang.Object)")
  def get(x1 : java.lang.Object) : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map.put((x$1:java.lang.Object, x$2:java.lang.Object)java.lang.Object)")
  def put(x1 : java.lang.Object, x2 : java.lang.Object) : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map.remove((x$1:java.lang.Object)java.lang.Object)")
  def remove(x1 : java.lang.Object) : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map.size(()Int)")
  def size() : Int = { 0 }

}
