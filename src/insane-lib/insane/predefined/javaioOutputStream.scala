package insane
package predefined

import annotations._

@AbstractsClass("java.io.OutputStream")
class javaioOutputStream {
  @AbstractsMethod("java.io.OutputStream.flush(()Unit)")
  def __flush(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.OutputStream.write((x$1: Array[Byte], x$2: Int, x$3: Int)Unit)")
  def __write(x1: Array[Byte], x2: Int, x3: Int): Unit = {
    ()
  }
}
