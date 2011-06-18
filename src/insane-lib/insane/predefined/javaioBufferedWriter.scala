package insane
package predefined

import annotations._

@AbstractsClass("java.io.BufferedWriter")
class javaioBufferedWriter {
  @AbstractsMethod("java.io.BufferedWriter.<init>((x$1: java.io.Writer)java.io.BufferedWriter)")
  def __init__(x1: java.io.Writer): java.io.BufferedWriter = {
    new java.io.BufferedWriter()
  }
}
