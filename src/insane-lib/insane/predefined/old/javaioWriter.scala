package insane
package predefined

import annotations._

@AbstractsClass("java.io.Writer")
class javaioWriter {
  @AbstractsMethod("java.io.Writer.close(()Unit)")
  def __close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.Writer.write((x$1: java.lang.String)Unit)")
  def __write(x1: java.lang.String): Unit = {
    ()
  }
}
