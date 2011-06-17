package insane
package predefined

import annotations._

@AbstractsClass("java.io.InputStream")
class javaioInputStream {
  @AbstractsMethod("java.io.InputStream.close(()Unit)")
  def close() : Unit = { () }

  @AbstractsMethod("java.io.InputStream.mark((x$1:Int)Unit)")
  def mark(x1 : Int) : Unit = { () }

  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1:java.io.InputStream)java.io.InputStreamReader)")
  def PLOPINIT(x1 : java.io.InputStream) : java.io.InputStreamReader = { new java.io.InputStreamReader() }

  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1:java.io.InputStream, x$2:java.lang.String)java.io.InputStreamReader)")
  def PLOPINIT(x1 : java.io.InputStream, x2 : java.lang.String) : java.io.InputStreamReader = { new java.io.InputStreamReader() }

  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1:java.io.InputStream, x$2:java.nio.charset.CharsetDecoder)java.io.InputStreamReader)")
  def PLOPINIT(x1 : java.io.InputStream, x2 : java.nio.charset.CharsetDecoder) : java.io.InputStreamReader = { new java.io.InputStreamReader() }

  @AbstractsMethod("java.io.InputStreamReader.read((x$1:Array[Char], x$2:Int, x$3:Int)Int)")
  def read(x1 : Array[Char], x2 : Int, x3 : Int) : Int = { 0 }

  @AbstractsMethod("java.io.InputStream.read(()Int)")
  def read() : Int = { 0 }

  @AbstractsMethod("java.io.InputStream.read((x$1:Array[Byte])Int)")
  def read(x1 : Array[Byte]) : Int = { 0 }

  @AbstractsMethod("java.io.InputStream.read((x$1:Array[Byte], x$2:Int, x$3:Int)Int)")
  def read(x1 : Array[Byte], x2 : Int, x3 : Int) : Int = { 0 }

  @AbstractsMethod("java.io.InputStream.reset(()Unit)")
  def reset() : Unit = { () }

}
