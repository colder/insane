package insane
package predefined

import annotations._

@AbstractsClass("java.io.StringReader")
class javaioStringReader {
  @AbstractsMethod("java.io.StringReader.<init>((x$1: java.lang.String)java.io.StringReader)")
  def __init__(x1: java.lang.String): java.io.StringReader = {
    new java.io.StringReader()
  }
}
