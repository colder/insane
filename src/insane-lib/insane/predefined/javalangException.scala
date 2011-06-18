package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Exception")
class javalangException {
  @AbstractsMethod("java.lang.Exception.<init>(()java.lang.Exception)")
  def __init__(): javalangException = {
    this
  }
  @AbstractsMethod("java.lang.Exception.<init>((x$1: java.lang.String)java.lang.Exception)")
  def __init__(x1: java.lang.String): javalangException = {
    this
  }
}
