package insane
package predefined

import annotations._

@AbstractsClass("scala.concurrent.forkjoin.ForkJoinTask")
abstract class scalaconcurrentforkjoinForkJoinTask {
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.fork(()Unit)")
  def __fork(): Unit = {
    ()
  }
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.join(()java.lang.Object)")
  def __join(): java.lang.Object
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.tryUnfork(()Boolean)")
  def __tryUnfork(): Boolean = {
    true
  }
}
