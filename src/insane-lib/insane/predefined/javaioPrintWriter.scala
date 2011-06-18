package insane
package predefined

import annotations._

@AbstractsClass("java.io.PrintWriter")
class javaioPrintWriter {
  @AbstractsMethod("java.io.PrintWriter.close(()Unit)")
  def close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintWriter.flush(()Unit)")
  def flush(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintWriter.<init>((x$1: java.io.OutputStream, x$2: Boolean)java.io.PrintWriter)")
  def __init__(x1: java.io.OutputStream, x2: Boolean): javaioPrintWriter = {
    this
  }
  @AbstractsMethod("java.io.PrintWriter.<init>((x$1: java.io.Writer)java.io.PrintWriter)")
  def __init__(x1: java.io.Writer): javaioPrintWriter = {
    this
  }
  @AbstractsMethod("java.io.PrintWriter.println((x$1: java.lang.String)Unit)")
  def println(x1: java.lang.String): Unit = {
    ()
  }
}
