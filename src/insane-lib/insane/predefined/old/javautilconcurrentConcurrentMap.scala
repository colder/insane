package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.ConcurrentMap")
abstract class javautilconcurrentConcurrentMap {
  @AbstractsMethod("java.util.concurrent.ConcurrentMap.putIfAbsent((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def __putIfAbsent(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.concurrent.ConcurrentMap.remove((x$1: java.lang.Object, x$2: java.lang.Object)Boolean)")
  def __remove(x1: java.lang.Object, x2: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.util.concurrent.ConcurrentMap.replace((x$1: java.lang.Object, x$2: java.lang.Object)java.lang.Object)")
  def __replace(x1: java.lang.Object, x2: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.util.concurrent.ConcurrentMap.replace((x$1: java.lang.Object, x$2: java.lang.Object, x$3: java.lang.Object)Boolean)")
  def __replace(x1: java.lang.Object, x2: java.lang.Object, x3: java.lang.Object): Boolean = {
    true
  }
}
