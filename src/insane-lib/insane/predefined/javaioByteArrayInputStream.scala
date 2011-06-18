package insane
package predefined

import annotations._

@AbstractsClass("java.io.ByteArrayInputStream")
class javaioByteArrayInputStream {
  @AbstractsMethod("java.io.ByteArrayInputStream.<init>((x$1: Array[Byte])java.io.ByteArrayInputStream)")
  def __init__(x1: Array[Byte]): java.io.ByteArrayInputStream = {
    new java.io.ByteArrayInputStream()
  }
}
