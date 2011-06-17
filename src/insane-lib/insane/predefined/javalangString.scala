package insane
package predefined

import annotations._

@AbstractsClass("java.lang.String")
class javalangString {
  @AbstractsMethod("java.lang.StringBuffer.<init>(()java.lang.StringBuffer)")
  def PLOPINIT() : java.lang.StringBuffer = { new java.lang.StringBuffer() }

  @AbstractsMethod("java.lang.StringBuffer.<init>((x$1:java.lang.CharSequence)java.lang.StringBuffer)")
  def PLOPINIT(x1 : java.lang.CharSequence) : java.lang.StringBuffer = { new java.lang.StringBuffer() }

  @AbstractsMethod("java.lang.StringBuffer.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Array[Char])java.lang.StringBuilder)")
  def append(x1 : Array[Char]) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Array[Char], x$2:Int, x$3:Int)java.lang.StringBuilder)")
  def append(x1 : Array[Char], x2 : Int, x3 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Boolean)java.lang.StringBuilder)")
  def append(x1 : Boolean) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Char)java.lang.StringBuilder)")
  def append(x1 : Char) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Double)java.lang.StringBuilder)")
  def append(x1 : Double) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Float)java.lang.StringBuilder)")
  def append(x1 : Float) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Int)java.lang.StringBuilder)")
  def append(x1 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:java.lang.CharSequence)java.lang.StringBuilder)")
  def append(x1 : java.lang.CharSequence) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:java.lang.String)java.lang.StringBuilder)")
  def append(x1 : java.lang.String) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.append((x$1:Long)java.lang.StringBuilder)")
  def append(x1 : Long) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.capacity(()Int)")
  def capacity() : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.charAt((x$1:Int)Char)")
  def charAt(x1 : Int) : Char = { '0' }

  @AbstractsMethod("java.lang.StringBuilder.deleteCharAt((x$1:Int)java.lang.StringBuilder)")
  def deleteCharAt(x1 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.delete((x$1:Int, x$2:Int)java.lang.StringBuilder)")
  def delete(x1 : Int, x2 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.ensureCapacity((x$1:Int)Unit)")
  def ensureCapacity(x1 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.StringBuilder.getChars((x$1:Int, x$2:Int, x$3:Array[Char], x$4:Int)Unit)")
  def getChars(x1 : Int, x2 : Int, x3 : Array[Char], x4 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.StringBuilder.indexOf((x$1:java.lang.String)Int)")
  def indexOf(x1 : java.lang.String) : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.indexOf((x$1:java.lang.String, x$2:Int)Int)")
  def indexOf(x1 : java.lang.String, x2 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.<init>((x$1:Int)java.lang.StringBuilder)")
  def PLOPINIT(x1 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.<init>((x$1:java.lang.CharSequence)java.lang.StringBuilder)")
  def PLOPINIT(x1 : java.lang.CharSequence) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.insert((x$1:Int, x$2:Array[Char])java.lang.StringBuilder)")
  def insert(x1 : Int, x2 : Array[Char]) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.insert((x$1:Int, x$2:Array[Char], x$3:Int, x$4:Int)java.lang.StringBuilder)")
  def insert(x1 : Int, x2 : Array[Char], x3 : Int, x4 : Int) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.insert((x$1:Int, x$2:java.lang.String)java.lang.StringBuilder)")
  def insert(x1 : Int, x2 : java.lang.String) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.lastIndexOf((x$1:java.lang.String)Int)")
  def lastIndexOf(x1 : java.lang.String) : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.lastIndexOf((x$1:java.lang.String, x$2:Int)Int)")
  def lastIndexOf(x1 : java.lang.String, x2 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.length(()Int)")
  def length() : Int = { 0 }

  @AbstractsMethod("java.lang.StringBuilder.replace((x$1:Int, x$2:Int, x$3:java.lang.String)java.lang.StringBuilder)")
  def replace(x1 : Int, x2 : Int, x3 : java.lang.String) : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.reverse(()java.lang.StringBuilder)")
  def reverse() : java.lang.StringBuilder = { new java.lang.StringBuilder() }

  @AbstractsMethod("java.lang.StringBuilder.setCharAt((x$1:Int, x$2:Char)Unit)")
  def setCharAt(x1 : Int, x2 : Char) : Unit = { () }

  @AbstractsMethod("java.lang.StringBuilder.setLength((x$1:Int)Unit)")
  def setLength(x1 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.StringBuilder.substring((x$1:Int, x$2:Int)java.lang.String)")
  def substring(x1 : Int, x2 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.StringBuilder.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.charAt((x$1:Int)Char)")
  def charAt(x1 : Int) : Char = { '0' }

  @AbstractsMethod("java.lang.String.compareTo((x$1:java.lang.String)Int)")
  def compareTo(x1 : java.lang.String) : Int = { 0 }

  @AbstractsMethod("java.lang.String.contains((x$1:java.lang.CharSequence)Boolean)")
  def contains(x1 : java.lang.CharSequence) : Boolean = { true }

  @AbstractsMethod("java.lang.String.endsWith((x$1:java.lang.String)Boolean)")
  def endsWith(x1 : java.lang.String) : Boolean = { true }

  @AbstractsMethod("java.lang.String.equals((x$1:java.lang.Object)Boolean)")
  def equals(x1 : java.lang.Object) : Boolean = { true }

  @AbstractsMethod("java.lang.String.format((x$1:java.lang.String, x$2:Array[java.lang.Object])java.lang.String)")
  def format(x1 : java.lang.String, x2 : Array[java.lang.Object]) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.format((x$1:java.util.Locale, x$2:java.lang.String, x$3:Array[java.lang.Object])java.lang.String)")
  def format(x1 : java.util.Locale, x2 : java.lang.String, x3 : Array[java.lang.Object]) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.getBytes((x$1:java.lang.String)Array[Byte])")
  def getBytes(x1 : java.lang.String) : Array[Byte] = { new Array[Byte]() }

  @AbstractsMethod("java.lang.String.getChars((x$1:Int, x$2:Int, x$3:Array[Char], x$4:Int)Unit)")
  def getChars(x1 : Int, x2 : Int, x3 : Array[Char], x4 : Int) : Unit = { () }

  @AbstractsMethod("java.lang.String.indexOf((x$1:Int)Int)")
  def indexOf(x1 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.String.indexOf((x$1:Int, x$2:Int)Int)")
  def indexOf(x1 : Int, x2 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.String.<init>((x$1:Array[Byte], x$2:Int, x$3:Int, x$4:java.lang.String)java.lang.String)")
  def PLOPINIT(x1 : Array[Byte], x2 : Int, x3 : Int, x4 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.<init>((x$1:Array[Byte], x$2:java.lang.String)java.lang.String)")
  def PLOPINIT(x1 : Array[Byte], x2 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.<init>((x$1:Array[Char])java.lang.String)")
  def PLOPINIT(x1 : Array[Char]) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.<init>((x$1:Array[Int], x$2:Int, x$3:Int)java.lang.String)")
  def PLOPINIT(x1 : Array[Int], x2 : Int, x3 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.lastIndexOf((x$1:Int)Int)")
  def lastIndexOf(x1 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.String.lastIndexOf((x$1:Int, x$2:Int)Int)")
  def lastIndexOf(x1 : Int, x2 : Int) : Int = { 0 }

  @AbstractsMethod("java.lang.String.lastIndexOf((x$1:java.lang.String)Int)")
  def lastIndexOf(x1 : java.lang.String) : Int = { 0 }

  @AbstractsMethod("java.lang.String.length(()Int)")
  def length() : Int = { 0 }

  @AbstractsMethod("java.lang.String.matches((x$1:java.lang.String)Boolean)")
  def matches(x1 : java.lang.String) : Boolean = { true }

  @AbstractsMethod("java.lang.String.$plus((x$1:java.lang.Object)java.lang.String)")
  def $plus(x1 : java.lang.Object) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.replaceAll((x$1:java.lang.String, x$2:java.lang.String)java.lang.String)")
  def replaceAll(x1 : java.lang.String, x2 : java.lang.String) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.split((x$1:java.lang.String)Array[java.lang.String])")
  def split(x1 : java.lang.String) : Array[java.lang.String] = { new Array[java.lang.String]() }

  @AbstractsMethod("java.lang.String.startsWith((x$1:java.lang.String)Boolean)")
  def startsWith(x1 : java.lang.String) : Boolean = { true }

  @AbstractsMethod("java.lang.String.startsWith((x$1:java.lang.String, x$2:Int)Boolean)")
  def startsWith(x1 : java.lang.String, x2 : Int) : Boolean = { true }

  @AbstractsMethod("java.lang.String.substring((x$1:Int)java.lang.String)")
  def substring(x1 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.substring((x$1:Int, x$2:Int)java.lang.String)")
  def substring(x1 : Int, x2 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.toCharArray(()Array[Char])")
  def toCharArray() : Array[Char] = { new Array[Char]() }

  @AbstractsMethod("java.lang.String.toLowerCase(()java.lang.String)")
  def toLowerCase() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.trim(()java.lang.String)")
  def trim() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Boolean)java.lang.String)")
  def valueOf(x1 : Boolean) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Char)java.lang.String)")
  def valueOf(x1 : Char) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Double)java.lang.String)")
  def valueOf(x1 : Double) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Float)java.lang.String)")
  def valueOf(x1 : Float) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Int)java.lang.String)")
  def valueOf(x1 : Int) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:java.lang.Object)java.lang.String)")
  def valueOf(x1 : java.lang.Object) : java.lang.String = { "" }

  @AbstractsMethod("java.lang.String.valueOf((x$1:Long)java.lang.String)")
  def valueOf(x1 : Long) : java.lang.String = { "" }

}
