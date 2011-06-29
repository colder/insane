package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Boolean")
class javalangBoolean {
  @AbstractsMethod("java.lang.Boolean.booleanValue(()Boolean)")
  def __booleanValue(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Boolean.hashCode(()Int)")
  def __hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Boolean.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Boolean.valueOf((x$1: Boolean)java.lang.Boolean)")
  def __valueOf(x1: Boolean): java.lang.Boolean = {
    new java.lang.Boolean(true)
  }
}
