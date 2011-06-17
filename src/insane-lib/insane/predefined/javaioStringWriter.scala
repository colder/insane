package insane
package predefined

import annotations._

@AbstractsClass("java.io.StringWriter")
class javaioStringWriter {
  @AbstractsMethod("java.io.StringWriter.<init>(()java.io.StringWriter)")
  def PLOPINIT() : java.io.StringWriter = { new java.io.StringWriter() }

  @AbstractsMethod("java.io.StringWriter.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

}
