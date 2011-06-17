package insane
package predefined

import annotations._

@AbstractsClass("scala.concurrent.forkjoin.ForkJoinTask")
class scalaconcurrentforkjoinForkJoinTask {
  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.fork(()Unit)")
  def fork() : Unit = { () }

  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.join(()java.lang.Object)")
  def join() : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("scala.concurrent.forkjoin.ForkJoinTask.tryUnfork(()Boolean)")
  def tryUnfork() : Boolean = { true }

}
