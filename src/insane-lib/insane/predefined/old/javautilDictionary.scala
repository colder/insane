package insane
package predefined

import annotations._

@AbstractsClass("java.util.Dictionary")
abstract class javautilDictionary {
  @AbstractsMethod("java.util.Dictionary.get((x$1: java.lang.Object)java.lang.Object)")
  def __get(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Dictionary.<init>(()java.util.Dictionary)")
  def ____init__(): javautilDictionary = {
    this
  }
  @AbstractsMethod("java.util.Dictionary.keys(()java.util.Enumeration)")
  def __keys(): java.util.Enumeration[_]
  @AbstractsMethod("java.util.Dictionary.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def __put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Dictionary.remove((x$1: java.lang.Object)java.lang.Object)")
  def __remove(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.Dictionary.size(()Int)")
  def __size(): Int = {
    42
  }
}
