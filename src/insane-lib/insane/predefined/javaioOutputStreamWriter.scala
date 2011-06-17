package insane
package predefined

import annotations._

@AbstractsClass("java.io.OutputStreamWriter")
class javaioOutputStreamWriter {
  @AbstractsMethod("java.io.OutputStreamWriter.flush(()Unit)")
  def flush() : Unit = { () }

  @AbstractsMethod("java.io.OutputStreamWriter.<init>((x$1:java.io.OutputStream)java.io.OutputStreamWriter)")
  def PLOPINIT(x1 : java.io.OutputStream) : java.io.OutputStreamWriter = { new java.io.OutputStreamWriter() }

  @AbstractsMethod("java.io.OutputStreamWriter.<init>((x$1:java.io.OutputStream, x$2:java.lang.String)java.io.OutputStreamWriter)")
  def PLOPINIT(x1 : java.io.OutputStream, x2 : java.lang.String) : java.io.OutputStreamWriter = { new java.io.OutputStreamWriter() }

  @AbstractsMethod("java.io.OutputStreamWriter.write((x$1:Array[Char], x$2:Int, x$3:Int)Unit)")
  def write(x1 : Array[Char], x2 : Int, x3 : Int) : Unit = { () }

  @AbstractsMethod("java.io.OutputStreamWriter.write((x$1:Int)Unit)")
  def write(x1 : Int) : Unit = { () }

}
