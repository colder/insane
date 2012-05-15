package insane
package predefined

import annotations._

@AbstractsClass("java.io.InputStreamReader")
class javaioInputStreamReader {
  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1: java.io.InputStream)java.io.InputStreamReader)")
  def ____init__(x1: java.io.InputStream): javaioInputStreamReader = {
    this
  }
  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1: java.io.InputStream, x$2: java.lang.String)java.io.InputStreamReader)")
  def ____init__(x1: java.io.InputStream, x2: java.lang.String): javaioInputStreamReader = {
    this
  }
  @AbstractsMethod("java.io.InputStreamReader.<init>((x$1: java.io.InputStream, x$2: java.nio.charset.CharsetDecoder)java.io.InputStreamReader)")
  def ____init__(x1: java.io.InputStream, x2: java.nio.charset.CharsetDecoder): javaioInputStreamReader = {
    this
  }
  @AbstractsMethod("java.io.InputStreamReader.read((x$1: Array[Char], x$2: Int, x$3: Int)Int)")
  def __read(x1: Array[Char], x2: Int, x3: Int): Int = {
    42
  }
}
