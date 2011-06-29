package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.ExecutorService")
abstract class javautilconcurrentExecutorService {
  @AbstractsMethod("java.util.concurrent.ExecutorService.shutdown(()Unit)")
  def __shutdown(): Unit = {
    ()
  }
  @AbstractsMethod("java.util.concurrent.ExecutorService.submit((x$1: java.util.concurrent.Callable)java.util.concurrent.Future)")
  def __submit(x1: java.util.concurrent.Callable[_]): java.util.concurrent.Future[_]
}
