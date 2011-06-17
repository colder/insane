package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.ExecutorService")
class javautilconcurrentExecutorService {
  @AbstractsMethod("java.util.concurrent.ExecutorService.shutdown(()Unit)")
  def shutdown() : Unit = { () }

  @AbstractsMethod("java.util.concurrent.ExecutorService.submit((x$1:java.util.concurrent.Callable)java.util.concurrent.Future)")
  def submit(x1 : java.util.concurrent.Callable) : java.util.concurrent.Future = { new java.util.concurrent.Future() }

}
