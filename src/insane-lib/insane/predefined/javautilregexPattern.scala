package insane
package predefined

import annotations._

@AbstractsClass("java.util.regex.Pattern")
abstract class javautilregexPattern {
  @AbstractsMethod("java.util.regex.Pattern.compile((x$1: java.lang.String)java.util.regex.Pattern)")
  def __compile(x1: java.lang.String): java.util.regex.Pattern
  @AbstractsMethod("java.util.regex.Pattern.matcher((x$1: java.lang.CharSequence)java.util.regex.Matcher)")
  def __matcher(x1: java.lang.CharSequence): java.util.regex.Matcher
  @AbstractsMethod("java.util.regex.Pattern.quote((x$1: java.lang.String)java.lang.String)")
  def __quote(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Pattern.split((x$1: java.lang.CharSequence)Array[java.lang.String])")
  def __split(x1: java.lang.CharSequence): Array[java.lang.String] = {
    new Array[java.lang.String](1)
  }
}
