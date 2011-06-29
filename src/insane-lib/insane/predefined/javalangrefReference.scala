package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.Reference")
abstract class javalangrefReference {
  @AbstractsMethod("java.lang.ref.Reference.clear(()Unit)")
  def __clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.ref.Reference.enqueue(()Boolean)")
  def __enqueue(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.ref.Reference.get(()java.lang.Object)")
  def __get(): java.lang.Object
  @AbstractsMethod("java.lang.ref.Reference.isEnqueued(()Boolean)")
  def __isEnqueued(): Boolean = {
    true
  }
}
