package insane
package predefined

import annotations._

@AbstractsClass("java.lang.String")
class javalangString {
  @AbstractsMethod("java.lang.String.charAt((x$1: Int)Char)")
  def __charAt(x1: Int): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.String.compareTo((x$1: java.lang.String)Int)")
  def __compareTo(x1: java.lang.String): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.contains((x$1: java.lang.CharSequence)Boolean)")
  def __contains(x1: java.lang.CharSequence): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.endsWith((x$1: java.lang.String)Boolean)")
  def __endsWith(x1: java.lang.String): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.equals((x$1: java.lang.Object)Boolean)")
  def __equals(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.format((x$1: java.lang.String, x$2: Array[java.lang.Object])java.lang.String)")
  def __format(x1: java.lang.String, x2: Array[java.lang.Object]): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.format((x$1: java.util.Locale, x$2: java.lang.String, x$3: Array[java.lang.Object])java.lang.String)")
  def __format(x1: java.util.Locale, x2: java.lang.String, x3: Array[java.lang.Object]): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.getBytes((x$1: java.lang.String)Array[Byte])")
  def __getBytes(x1: java.lang.String): Array[Byte] = {
    new Array[Byte](1)
  }
  @AbstractsMethod("java.lang.String.getChars((x$1: Int, x$2: Int, x$3: Array[Char], x$4: Int)Unit)")
  def __getChars(x1: Int, x2: Int, x3: Array[Char], x4: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.String.indexOf((x$1: Int)Int)")
  def __indexOf(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.indexOf((x$1: Int, x$2: Int)Int)")
  def __indexOf(x1: Int, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.<init>((x$1: Array[Byte], x$2: Int, x$3: Int, x$4: java.lang.String)java.lang.String)")
  def ____init__(x1: Array[Byte], x2: Int, x3: Int, x4: java.lang.String): javalangString = {
    this
  }
  @AbstractsMethod("java.lang.String.<init>((x$1: Array[Byte], x$2: java.lang.String)java.lang.String)")
  def ____init__(x1: Array[Byte], x2: java.lang.String): javalangString = {
    this
  }
  @AbstractsMethod("java.lang.String.<init>((x$1: Array[Char])java.lang.String)")
  def ____init__(x1: Array[Char]): javalangString = {
    this
  }
  @AbstractsMethod("java.lang.String.<init>((x$1: Array[Int], x$2: Int, x$3: Int)java.lang.String)")
  def ____init__(x1: Array[Int], x2: Int, x3: Int): javalangString = {
    this
  }
  @AbstractsMethod("java.lang.String.lastIndexOf((x$1: Int)Int)")
  def __lastIndexOf(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.lastIndexOf((x$1: Int, x$2: Int)Int)")
  def __lastIndexOf(x1: Int, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.lastIndexOf((x$1: java.lang.String)Int)")
  def __lastIndexOf(x1: java.lang.String): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.length(()Int)")
  def __length(): Int = {
    42
  }
  @AbstractsMethod("java.lang.String.matches((x$1: java.lang.String)Boolean)")
  def __matches(x1: java.lang.String): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.$plus((x$1: java.lang.Object)java.lang.String)")
  def __$plus(x1: java.lang.Object): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.replaceAll((x$1: java.lang.String, x$2: java.lang.String)java.lang.String)")
  def __replaceAll(x1: java.lang.String, x2: java.lang.String): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.split((x$1: java.lang.String)Array[java.lang.String])")
  def __split(x1: java.lang.String): Array[java.lang.String] = {
    new Array[java.lang.String](1)
  }
  @AbstractsMethod("java.lang.String.startsWith((x$1: java.lang.String)Boolean)")
  def __startsWith(x1: java.lang.String): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.startsWith((x$1: java.lang.String, x$2: Int)Boolean)")
  def __startsWith(x1: java.lang.String, x2: Int): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.String.substring((x$1: Int)java.lang.String)")
  def __substring(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.substring((x$1: Int, x$2: Int)java.lang.String)")
  def __substring(x1: Int, x2: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.toCharArray(()Array[Char])")
  def __toCharArray(): Array[Char] = {
    new Array[Char](1)
  }
  @AbstractsMethod("java.lang.String.toLowerCase(()java.lang.String)")
  def __toLowerCase(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.trim(()java.lang.String)")
  def __trim(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Boolean)java.lang.String)")
  def __valueOf(x1: Boolean): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Char)java.lang.String)")
  def __valueOf(x1: Char): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Double)java.lang.String)")
  def __valueOf(x1: Double): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Float)java.lang.String)")
  def __valueOf(x1: Float): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Int)java.lang.String)")
  def __valueOf(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: java.lang.Object)java.lang.String)")
  def __valueOf(x1: java.lang.Object): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.String.valueOf((x$1: Long)java.lang.String)")
  def __valueOf(x1: Long): java.lang.String = {
    ""
  }
}
