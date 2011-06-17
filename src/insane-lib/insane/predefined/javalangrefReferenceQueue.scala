package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.ReferenceQueue")
class javalangrefReferenceQueue {
  @AbstractsMethod("java.lang.ref.ReferenceQueue.<init>(()java.lang.ref.ReferenceQueue)")
  def PLOPINIT() : java.lang.ref.ReferenceQueue = { new java.lang.ref.ReferenceQueue() }

  @AbstractsMethod("java.lang.ref.ReferenceQueue.poll(()java.lang.ref.Reference)")
  def poll() : java.lang.ref.Reference = { new java.lang.ref.Reference() }

  @AbstractsMethod("java.lang.ref.ReferenceQueue.remove(()java.lang.ref.Reference)")
  def remove() : java.lang.ref.Reference = { new java.lang.ref.Reference() }

  @AbstractsMethod("java.lang.ref.ReferenceQueue.remove((x$1:Long)java.lang.ref.Reference)")
  def remove(x1 : Long) : java.lang.ref.Reference = { new java.lang.ref.Reference() }

}
