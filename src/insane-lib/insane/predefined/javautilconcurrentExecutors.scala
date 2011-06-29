package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.Executors")
abstract class javautilconcurrentExecutors {
  @AbstractsMethod("java.util.concurrent.Executors.newCachedThreadPool(()java.util.concurrent.ExecutorService)")
  def __newCachedThreadPool(): java.util.concurrent.ExecutorService
}
