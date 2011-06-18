package insane
package predefined

import annotations._

@AbstractsClass("java.io.Writer")
class javaioWriter {
  @AbstractsMethod("java.io.Writer.close(()Unit)")
  def close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.Writer.write((x$1: java.lang.String)Unit)")
  def write(x1: java.lang.String): Unit = {
    ()
  }
}
