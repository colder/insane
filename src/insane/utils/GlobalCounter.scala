package insane
package utils

object GlobalCounter {
  private var cnt = 0

  def withDebugCounter(f: Int => Unit) {
    cnt +=1
    f(cnt)
  }
}
