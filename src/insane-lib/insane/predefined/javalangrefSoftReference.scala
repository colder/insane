package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ref.SoftReference")
class javalangrefSoftReference {
  @AbstractsMethod("java.lang.ref.SoftReference.<init>((x$1:java.lang.Object, x$2:java.lang.ref.ReferenceQueue)java.lang.ref.SoftReference)")
  def PLOPINIT(x1 : java.lang.Object, x2 : java.lang.ref.ReferenceQueue) : java.lang.ref.SoftReference = { new java.lang.ref.SoftReference() }

}
