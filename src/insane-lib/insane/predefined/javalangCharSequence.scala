package insane
package predefined

import annotations._

@AbstractsClass("java.lang.CharSequence")
class javalangCharSequence {
  @AbstractsMethod("java.lang.CharSequence.charAt((x$1:Int)Char)")
  def charAt(x1 : Int) : Char = { '0' }

  @AbstractsMethod("java.lang.CharSequence.length(()Int)")
  def length() : Int = { 0 }

  @AbstractsMethod("java.lang.CharSequence.subSequence((x$1:Int, x$2:Int)java.lang.CharSequence)")
  def subSequence(x1 : Int, x2 : Int) : java.lang.CharSequence = { new java.lang.CharSequence() }

  @AbstractsMethod("java.lang.CharSequence.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

}
