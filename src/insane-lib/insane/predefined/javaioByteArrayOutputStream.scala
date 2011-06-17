package insane
package predefined

import annotations._

@AbstractsClass("java.io.ByteArrayOutputStream")
class javaioByteArrayOutputStream {
  @AbstractsMethod("java.io.ByteArrayOutputStream.<init>((x$1:Int)java.io.ByteArrayOutputStream)")
  def PLOPINIT(x1 : Int) : java.io.ByteArrayOutputStream = { new java.io.ByteArrayOutputStream() }

  @AbstractsMethod("java.io.ByteArrayOutputStream.toByteArray(()Array[Byte])")
  def toByteArray() : Array[Byte] = { new Array[Byte]() }

}
