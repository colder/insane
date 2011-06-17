package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.AbstractExecutorService")
class javautilconcurrentAbstractExecutorService {
  @AbstractsMethod("java.util.concurrent.AbstractExecutorService.submit((x$1:java.lang.Runnable)java.util.concurrent.Future)")
  def submit(x1 : java.lang.Runnable) : java.util.concurrent.Future = { new java.util.concurrent.Future() }

}
