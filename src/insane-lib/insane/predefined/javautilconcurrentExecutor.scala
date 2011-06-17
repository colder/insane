package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.Executor")
class javautilconcurrentExecutor {
  @AbstractsMethod("java.util.concurrent.Executor.execute((x$1:java.lang.Runnable)Unit)")
  def execute(x1 : java.lang.Runnable) : Unit = { () }

  @AbstractsMethod("java.util.concurrent.ExecutorService.shutdown(()Unit)")
  def shutdown() : Unit = { () }

  @AbstractsMethod("java.util.concurrent.ExecutorService.submit((x$1:java.util.concurrent.Callable)java.util.concurrent.Future)")
  def submit(x1 : java.util.concurrent.Callable) : java.util.concurrent.Future = { new java.util.concurrent.Future() }

  @AbstractsMethod("java.util.concurrent.Executors.newCachedThreadPool(()java.util.concurrent.ExecutorService)")
  def newCachedThreadPool() : java.util.concurrent.ExecutorService = { new java.util.concurrent.ExecutorService() }

}
