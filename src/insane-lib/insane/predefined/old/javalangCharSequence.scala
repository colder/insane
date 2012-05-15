package insane
package predefined

import annotations._

@AbstractsClass("java.lang.CharSequence")
abstract class javalangCharSequence {
  @AbstractsMethod("java.lang.CharSequence.charAt((x$1: Int)Char)")
  def __charAt(x1: Int): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.CharSequence.length(()Int)")
  def __length(): Int = {
    42
  }
  @AbstractsMethod("java.lang.CharSequence.subSequence((x$1: Int, x$2: Int)java.lang.CharSequence)")
  def __subSequence(x1: Int, x2: Int): java.lang.CharSequence
  @AbstractsMethod("java.lang.CharSequence.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
}
