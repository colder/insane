package insane
package predefined

import annotations._

@AbstractsClass("java.util.regex.Matcher")
class javautilregexMatcher {
  @AbstractsMethod("java.util.regex.Matcher.appendReplacement((x$1:java.lang.StringBuffer, x$2:java.lang.String)java.util.regex.Matcher)")
  def appendReplacement(x1 : java.lang.StringBuffer, x2 : java.lang.String) : java.util.regex.Matcher = { new java.util.regex.Matcher() }

  @AbstractsMethod("java.util.regex.Matcher.appendTail((x$1:java.lang.StringBuffer)java.lang.StringBuffer)")
  def appendTail(x1 : java.lang.StringBuffer) : java.lang.StringBuffer = { new java.lang.StringBuffer() }

  @AbstractsMethod("java.util.regex.Matcher.end(()Int)")
  def end() : Int = { 0 }

  @AbstractsMethod("java.util.regex.Matcher.end((x$1:Int)Int)")
  def end(x1 : Int) : Int = { 0 }

  @AbstractsMethod("java.util.regex.Matcher.find(()Boolean)")
  def find() : Boolean = { true }

  @AbstractsMethod("java.util.regex.Matcher.groupCount(()Int)")
  def groupCount() : Int = { 0 }

  @AbstractsMethod("java.util.regex.Matcher.group(()java.lang.String)")
  def group() : java.lang.String = { "" }

  @AbstractsMethod("java.util.regex.Matcher.group((x$1:Int)java.lang.String)")
  def group(x1 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.util.regex.Matcher.lookingAt(()Boolean)")
  def lookingAt() : Boolean = { true }

  @AbstractsMethod("java.util.regex.Matcher.matches(()Boolean)")
  def matches() : Boolean = { true }

  @AbstractsMethod("java.util.regex.Matcher.quoteReplacement((x$1:java.lang.String)java.lang.String)")
  def quoteReplacement(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.util.regex.Matcher.replaceAll((x$1:java.lang.String)java.lang.String)")
  def replaceAll(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.util.regex.Matcher.replaceFirst((x$1:java.lang.String)java.lang.String)")
  def replaceFirst(x1 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.util.regex.Matcher.start(()Int)")
  def start() : Int = { 0 }

  @AbstractsMethod("java.util.regex.Matcher.start((x$1:Int)Int)")
  def start(x1 : Int) : Int = { 0 }

}
