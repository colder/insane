package insane
package predefined

import annotations._

@AbstractsClass("java.lang.StringBuffer")
class javalangStringBuffer {
  @AbstractsMethod("java.lang.StringBuffer.<init>(()java.lang.StringBuffer)")
  def __init__(): javalangStringBuffer = {
    this
  }
  @AbstractsMethod("java.lang.StringBuffer.<init>((x$1: java.lang.CharSequence)java.lang.StringBuffer)")
  def __init__(x1: java.lang.CharSequence): javalangStringBuffer = {
    this
  }
  @AbstractsMethod("java.lang.StringBuffer.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
}
