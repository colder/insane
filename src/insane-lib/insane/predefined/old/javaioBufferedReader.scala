package insane
package predefined

import annotations._

@AbstractsClass("java.io.BufferedReader")
class javaioBufferedReader {
  @AbstractsMethod("java.io.BufferedReader.<init>((x$1: java.io.Reader)java.io.BufferedReader)")
  def ____init__(x1: java.io.Reader): javaioBufferedReader = {
    this
  }
  @AbstractsMethod("java.io.BufferedReader.<init>((x$1: java.io.Reader, x$2: Int)java.io.BufferedReader)")
  def ____init__(x1: java.io.Reader, x2: Int): javaioBufferedReader = {
    this
  }
  @AbstractsMethod("java.io.BufferedReader.read(()Int)")
  def __read(): Int = {
    42
  }
  @AbstractsMethod("java.io.BufferedReader.readLine(()java.lang.String)")
  def __readLine(): java.lang.String = {
    ""
  }
}
