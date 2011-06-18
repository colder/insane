package insane
package predefined

import annotations._

@AbstractsClass("java.util.regex.Pattern")
class javautilregexPattern {
  @AbstractsMethod("java.util.regex.Pattern.compile((x$1: java.lang.String)java.util.regex.Pattern)")
  def compile(x1: java.lang.String): java.util.regex.Pattern = {
    new java.util.regex.Pattern()
  }
  @AbstractsMethod("java.util.regex.Pattern.matcher((x$1: java.lang.CharSequence)java.util.regex.Matcher)")
  def matcher(x1: java.lang.CharSequence): java.util.regex.Matcher = {
    new java.util.regex.Matcher()
  }
  @AbstractsMethod("java.util.regex.Pattern.quote((x$1: java.lang.String)java.lang.String)")
  def quote(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Pattern.split((x$1: java.lang.CharSequence)Array[java.lang.String])")
  def split(x1: java.lang.CharSequence): Array[java.lang.String] = {
    new Array[java.lang.String](1)
  }
}
