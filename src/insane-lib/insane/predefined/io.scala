package insane
package insane.predefined

import annotations._

object io {
  @AltName("java.io.BufferedReader.<init>((x$1: java.io.Reader, x$2: Int)java.io.BufferedReader)")
  def bufferedReader_init(a: java.io.Reader, x2: Int): java.io.BufferedReader = {
    null
  }

  @AltName("java.io.BufferedReader.readLine(()java.lang.String)")
  def bufferedReader_ReadLine() = {
    "foo"
  }

  @AltName("java.io.File.getAbsolutePath(()java.lang.String)")
  def File_getAbsolutePath() = {
    "foo"
  }

  @AltName("java.io.FileInputStream.<init>((x$1: java.io.File)java.io.FileInputStream)")
  def FileInputStream_init(x: java.io.File): java.io.FileInputStream = {
    null
  }

  @AltName("java.io.FilterInputStream.<init>((x$1: java.io.InputStream)java.io.FilterInputStream)")
  def FilterInputStream_init(x: java.io.InputStream): java.io.FilterInputStream = {
    null
  }

  @AltName("java.io.FilterOutputStream.<init>((x$1: java.io.OutputStream)java.io.FilterOutputStream)")
  def FileOutputStream_init(x: java.io.File): java.io.FileOutputStream = {
    null
  }

  @AltName("java.io.InputStreamReader.<init>((x$1: java.io.InputStream, x$2: java.nio.charset.CharsetDecoder)java.io.InputStreamReader)")
  def InputStreamReader_init(x: java.io.InputStream, y: java.nio.charset.CharsetDecoder): java.io.InputStreamReader = {
    null
  }

  @AltName("java.io.InputStream.read((x$1: Array[Byte])Int)")
  def InputStream_read(x: Array[Byte]): Int = {
    42
  }

  @AltName("java.io.InputStream.read((x$1: Array[Byte], x$2: Int, x$3: Int)Int)")
  def InputStream_read(x: Array[Byte], i1: Int, i2: Int): Int = {
    42
  }

  @AltName("java.io.InputStream.reset(()Unit)")
  def InputStream_reset(): Unit = {
  }

  @AltName("java.io.IOException.<init>((x$1: java.lang.String)java.io.IOException)")
  def IOException_init(s: String): java.io.IOException = {
    null
  }

  @AltName("java.io.OutputStream.flush(()Unit)")
  def OutputStream_flush(): Unit = {

  }

  @AltName("java.io.OutputStream.write((x$1: Array[Byte], x$2: Int, x$3: Int)Unit)")
  def OutputStream_write(x: Array[Byte], x2: Int, x3: Int): Unit = {

  }

  @AltName("java.io.PrintStream.flush(()Unit)")
  def PrintStream_flush(): Unit = {

  }

  @AltName("java.io.PrintStream.println((x$1: java.lang.Object)Unit)")
  def OutputStream_println(x: java.lang.Object): Unit = {

  }
}
