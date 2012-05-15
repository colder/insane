package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.ThreadPoolExecutor")
abstract class javautilconcurrentThreadPoolExecutor {
  @AbstractsMethod("java.util.concurrent.ThreadPoolExecutor.getCorePoolSize(()Int)")
  def __getCorePoolSize(): Int = {
    42
  }
  @AbstractsMethod("java.util.concurrent.ThreadPoolExecutor.getQueue(()java.util.concurrent.BlockingQueue)")
  def __getQueue(): java.util.concurrent.BlockingQueue[_]
  @AbstractsMethod("java.util.concurrent.ThreadPoolExecutor.<init>((x$1: Int, x$2: Int, x$3: Long, x$4: java.util.concurrent.TimeUnit, x$5: java.util.concurrent.BlockingQueue, x$6: java.util.concurrent.RejectedExecutionHandler)java.util.concurrent.ThreadPoolExecutor)")
  def ____init__(x1: Int, x2: Int, x3: Long, x4: java.util.concurrent.TimeUnit, x5: java.util.concurrent.BlockingQueue[_], x6: java.util.concurrent.RejectedExecutionHandler): javautilconcurrentThreadPoolExecutor = {
    this
  }
  @AbstractsMethod("java.util.concurrent.ThreadPoolExecutor.<init>((x$1: Int, x$2: Int, x$3: Long, x$4: java.util.concurrent.TimeUnit, x$5: java.util.concurrent.BlockingQueue, x$6: java.util.concurrent.ThreadFactory, x$7: java.util.concurrent.RejectedExecutionHandler)java.util.concurrent.ThreadPoolExecutor)")
  def ____init__(x1: Int, x2: Int, x3: Long, x4: java.util.concurrent.TimeUnit, x5: java.util.concurrent.BlockingQueue[_], x6: java.util.concurrent.ThreadFactory, x7: java.util.concurrent.RejectedExecutionHandler): javautilconcurrentThreadPoolExecutor = {
    this
  }
  @AbstractsMethod("java.util.concurrent.ThreadPoolExecutor.setCorePoolSize((x$1: Int)Unit)")
  def __setCorePoolSize(x1: Int): Unit = {
    ()
  }
}
