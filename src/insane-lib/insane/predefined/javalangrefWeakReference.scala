package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.WeakReference")
class javalangrefWeakReference {
  @AbstractsMethod("java.lang.ref.WeakReference.<init>((x$1: java.lang.Object)java.lang.ref.WeakReference)")
  def ____init__(x1: java.lang.Object): javalangrefWeakReference = {
    this
  }
  @AbstractsMethod("java.lang.ref.WeakReference.<init>((x$1: java.lang.Object, x$2: java.lang.ref.ReferenceQueue)java.lang.ref.WeakReference)")
  def ____init__(x1: java.lang.Object, x2: java.lang.ref.ReferenceQueue[_]): javalangrefWeakReference = {
    this
  }
}
