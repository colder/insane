package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.ReferenceQueue")
abstract class javalangrefReferenceQueue {
  @AbstractsMethod("java.lang.ref.ReferenceQueue.<init>(()java.lang.ref.ReferenceQueue)")
  def __init__(): javalangrefReferenceQueue = {
    this
  }
  @AbstractsMethod("java.lang.ref.ReferenceQueue.poll(()java.lang.ref.Reference)")
  def poll(): java.lang.ref.Reference[_]
  @AbstractsMethod("java.lang.ref.ReferenceQueue.remove(()java.lang.ref.Reference)")
  def remove(): java.lang.ref.Reference[_]
  @AbstractsMethod("java.lang.ref.ReferenceQueue.remove((x$1: Long)java.lang.ref.Reference)")
  def remove(x1: Long): java.lang.ref.Reference[_]
}
