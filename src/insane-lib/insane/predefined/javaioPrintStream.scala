package insane
package predefined

import annotations._

@AbstractsClass("java.io.PrintStream")
class javaioPrintStream {
  @AbstractsMethod("java.io.PrintStream.flush(()Unit)")
  def __flush(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintStream.<init>((x$1: java.io.OutputStream)java.io.PrintStream)")
  def ____init__(x1: java.io.OutputStream): javaioPrintStream = {
    this
  }
  @AbstractsMethod("java.io.PrintStream.println(()Unit)")
  def __println(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintStream.println((x$1: java.lang.Object)Unit)")
  def __println(x1: java.lang.Object): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintStream.println((x$1: java.lang.String)Unit)")
  def __println(x1: java.lang.String): Unit = {
    ()
  }
  @AbstractsMethod("java.io.PrintStream.print((x$1: java.lang.String)Unit)")
  def __print(x1: java.lang.String): Unit = {
    ()
  }
}
