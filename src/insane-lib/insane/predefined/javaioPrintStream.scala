package insane
package predefined

import annotations._

@AbstractsClass("java.io.PrintStream")
class javaioPrintStream {
  @AbstractsMethod("java.io.PrintStream.flush(()Unit)")
  def flush() : Unit = { () }

  @AbstractsMethod("java.io.PrintStream.<init>((x$1:java.io.OutputStream)java.io.PrintStream)")
  def PLOPINIT(x1 : java.io.OutputStream) : java.io.PrintStream = { new java.io.PrintStream() }

  @AbstractsMethod("java.io.PrintStream.println(()Unit)")
  def println() : Unit = { () }

  @AbstractsMethod("java.io.PrintStream.println((x$1:java.lang.Object)Unit)")
  def println(x1 : java.lang.Object) : Unit = { () }

  @AbstractsMethod("java.io.PrintStream.println((x$1:java.lang.String)Unit)")
  def println(x1 : java.lang.String) : Unit = { () }

  @AbstractsMethod("java.io.PrintStream.print((x$1:java.lang.String)Unit)")
  def print(x1 : java.lang.String) : Unit = { () }

}
