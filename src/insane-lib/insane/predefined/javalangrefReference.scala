package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.Reference")
abstract class javalangrefReference {
  @AbstractsMethod("java.lang.ref.Reference.clear(()Unit)")
  def clear(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.ref.Reference.enqueue(()Boolean)")
  def enqueue(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.ref.Reference.get(()java.lang.Object)")
  def get(): java.lang.Object
  @AbstractsMethod("java.lang.ref.Reference.isEnqueued(()Boolean)")
  def isEnqueued(): Boolean = {
    true
  }
}
