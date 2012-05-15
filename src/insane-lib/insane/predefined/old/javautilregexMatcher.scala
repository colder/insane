package insane
package predefined

import annotations._

@AbstractsClass("java.util.regex.Matcher")
abstract class javautilregexMatcher {
  @AbstractsMethod("java.util.regex.Matcher.appendReplacement((x$1: java.lang.StringBuffer, x$2: java.lang.String)java.util.regex.Matcher)")
  def __appendReplacement(x1: java.lang.StringBuffer, x2: java.lang.String): java.util.regex.Matcher
  @AbstractsMethod("java.util.regex.Matcher.appendTail((x$1: java.lang.StringBuffer)java.lang.StringBuffer)")
  def __appendTail(x1: java.lang.StringBuffer): java.lang.StringBuffer
  @AbstractsMethod("java.util.regex.Matcher.end(()Int)")
  def __end(): Int = {
    42
  }
  @AbstractsMethod("java.util.regex.Matcher.end((x$1: Int)Int)")
  def __end(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.util.regex.Matcher.find(()Boolean)")
  def __find(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.regex.Matcher.groupCount(()Int)")
  def __groupCount(): Int = {
    42
  }
  @AbstractsMethod("java.util.regex.Matcher.group(()java.lang.String)")
  def __group(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Matcher.group((x$1: Int)java.lang.String)")
  def __group(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Matcher.lookingAt(()Boolean)")
  def __lookingAt(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.regex.Matcher.matches(()Boolean)")
  def __matches(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.regex.Matcher.quoteReplacement((x$1: java.lang.String)java.lang.String)")
  def __quoteReplacement(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Matcher.replaceAll((x$1: java.lang.String)java.lang.String)")
  def __replaceAll(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Matcher.replaceFirst((x$1: java.lang.String)java.lang.String)")
  def __replaceFirst(x1: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.util.regex.Matcher.start(()Int)")
  def __start(): Int = {
    42
  }
  @AbstractsMethod("java.util.regex.Matcher.start((x$1: Int)Int)")
  def __start(x1: Int): Int = {
    42
  }
}
