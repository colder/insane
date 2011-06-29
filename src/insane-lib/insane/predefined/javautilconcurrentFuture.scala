package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.Future")
abstract class javautilconcurrentFuture {
  @AbstractsMethod("java.util.concurrent.Future.get(()java.lang.Object)")
  def __get(): java.lang.Object
}
