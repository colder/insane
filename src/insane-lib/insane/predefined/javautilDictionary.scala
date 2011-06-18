package insane
package predefined

import annotations._

@AbstractsClass("java.util.Dictionary")
class javautilDictionary {
  @AbstractsMethod("java.util.Dictionary.get((x$1: java.lang.Object)java.lang.Object)")
  def get(x1: java.lang.Object): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.util.Dictionary.<init>(()java.util.Dictionary)")
  def __init__(): javautilDictionary = {
    this
  }
  @AbstractsMethod("java.util.Dictionary.keys(()java.util.Enumeration)")
  def keys(): java.util.Enumeration = {
    new java.util.Enumeration()
  }
  @AbstractsMethod("java.util.Dictionary.put((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def put(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.util.Dictionary.remove((x$1: java.lang.Object)java.lang.Object)")
  def remove(x1: java.lang.Object): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.util.Dictionary.size(()Int)")
  def size(): Int = {
    42
  }
}
