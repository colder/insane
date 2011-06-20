package insane
package predefined

import annotations._

@AbstractsClass("java.lang.StringBuilder")
abstract class javalangStringBuilder {
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Array[Char])java.lang.StringBuilder)")
  def append(x1: Array[Char]): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Array[Char], x$2: Int, x$3: Int)java.lang.StringBuilder)")
  def append(x1: Array[Char], x2: Int, x3: Int): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Boolean)java.lang.StringBuilder)")
  def append(x1: Boolean): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Char)java.lang.StringBuilder)")
  def append(x1: Char): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Double)java.lang.StringBuilder)")
  def append(x1: Double): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Float)java.lang.StringBuilder)")
  def append(x1: Float): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Int)java.lang.StringBuilder)")
  def append(x1: Int): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: java.lang.CharSequence)java.lang.StringBuilder)")
  def append(x1: java.lang.CharSequence): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: java.lang.String)java.lang.StringBuilder)")
  def append(x1: java.lang.String): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.append((x$1: Long)java.lang.StringBuilder)")
  def append(x1: Long): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.capacity(()Int)")
  def capacity(): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.charAt((x$1: Int)Char)")
  def charAt(x1: Int): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.StringBuilder.deleteCharAt((x$1: Int)java.lang.StringBuilder)")
  def deleteCharAt(x1: Int): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.delete((x$1: Int, x$2: Int)java.lang.StringBuilder)")
  def delete(x1: Int, x2: Int): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.ensureCapacity((x$1: Int)Unit)")
  def ensureCapacity(x1: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.StringBuilder.getChars((x$1: Int, x$2: Int, x$3: Array[Char], x$4: Int)Unit)")
  def getChars(x1: Int, x2: Int, x3: Array[Char], x4: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.StringBuilder.indexOf((x$1: java.lang.String)Int)")
  def indexOf(x1: java.lang.String): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.indexOf((x$1: java.lang.String, x$2: Int)Int)")
  def indexOf(x1: java.lang.String, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.<init>((x$1: Int)java.lang.StringBuilder)")
  def __init__(x1: Int): javalangStringBuilder = {
    this
  }
  @AbstractsMethod("java.lang.StringBuilder.<init>((x$1: java.lang.CharSequence)java.lang.StringBuilder)")
  def __init__(x1: java.lang.CharSequence): javalangStringBuilder = {
    this
  }
  @AbstractsMethod("java.lang.StringBuilder.insert((x$1: Int, x$2: Array[Char])java.lang.StringBuilder)")
  def insert(x1: Int, x2: Array[Char]): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.insert((x$1: Int, x$2: Array[Char], x$3: Int, x$4: Int)java.lang.StringBuilder)")
  def insert(x1: Int, x2: Array[Char], x3: Int, x4: Int): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.insert((x$1: Int, x$2: java.lang.String)java.lang.StringBuilder)")
  def insert(x1: Int, x2: java.lang.String): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.lastIndexOf((x$1: java.lang.String)Int)")
  def lastIndexOf(x1: java.lang.String): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.lastIndexOf((x$1: java.lang.String, x$2: Int)Int)")
  def lastIndexOf(x1: java.lang.String, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.length(()Int)")
  def length(): Int = {
    42
  }
  @AbstractsMethod("java.lang.StringBuilder.replace((x$1: Int, x$2: Int, x$3: java.lang.String)java.lang.StringBuilder)")
  def replace(x1: Int, x2: Int, x3: java.lang.String): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.reverse(()java.lang.StringBuilder)")
  def reverse(): java.lang.StringBuilder
  @AbstractsMethod("java.lang.StringBuilder.setCharAt((x$1: Int, x$2: Char)Unit)")
  def setCharAt(x1: Int, x2: Char): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.StringBuilder.setLength((x$1: Int)Unit)")
  def setLength(x1: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.StringBuilder.substring((x$1: Int, x$2: Int)java.lang.String)")
  def substring(x1: Int, x2: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.StringBuilder.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
}
