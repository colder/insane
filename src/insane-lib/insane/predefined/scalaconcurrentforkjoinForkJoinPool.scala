package insane
package predefined

import annotations._

@AbstractsClass("scala.concurrent.forkjoin.ForkJoinPool")
class scalaconcurrentforkjoinForkJoinPool {
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinPool.execute((x$1: scala.concurrent.forkjoin.ForkJoinTask)Unit)")
  def execute(x1: scala.concurrent.forkjoin.ForkJoinTask): Unit = {
    ()
  }
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinPool.getParallelism(()Int)")
  def getParallelism(): Int = {
    42
  }
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinPool.<init>(()scala.concurrent.forkjoin.ForkJoinPool)")
  def __init__(): scalaconcurrentforkjoinForkJoinPool = {
    this
  }
}
