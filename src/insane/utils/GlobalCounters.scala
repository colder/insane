package insane
package utils

object GlobalCounters {
  private var debugCounter = 0

  def withDebugCounter(f: Int => Unit) {
    f(getDebugCounter)
  }

  def getDebugCounter = {
    debugCounter +=1
    debugCounter
  }

  private var cfgCounter = 0

  def withCFGCounter(f: Int => Unit) {
    f(getCFGCounter)
  }

  def getCFGCounter = {
    cfgCounter +=1
    cfgCounter
  }
}
