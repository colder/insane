package insane
package predefined

import annotations._

@AbstractsClass("java.io.StringWriter")
class javaioStringWriter {
  @AbstractsMethod("java.io.StringWriter.<init>(()java.io.StringWriter)")
  def ____init__(): javaioStringWriter = {
    this
  }
  @AbstractsMethod("java.io.StringWriter.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
}
