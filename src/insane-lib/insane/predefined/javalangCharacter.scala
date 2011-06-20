package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Character")
class javalangCharacter {
  @AbstractsMethod("java.lang.Character.charValue(()Char)")
  def charValue(): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.digit((x$1: Char, x$2: Int)Int)")
  def digit(x1: Char, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getDirectionality((x$1: Char)Byte)")
  def getDirectionality(x1: Char): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Character.getNumericValue((x$1: Char)Int)")
  def getNumericValue(x1: Char): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getType((x$1: Char)Int)")
  def getType(x1: Char): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getType((x$1: Int)Int)")
  def getType(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.isDigit((x$1: Char)Boolean)")
  def isDigit(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isHighSurrogate((x$1: Char)Boolean)")
  def isHighSurrogate(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isIdentifierIgnorable((x$1: Char)Boolean)")
  def isIdentifierIgnorable(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isISOControl((x$1: Char)Boolean)")
  def isISOControl(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isJavaIdentifierPart((x$1: Char)Boolean)")
  def isJavaIdentifierPart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLetterOrDigit((x$1: Char)Boolean)")
  def isLetterOrDigit(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLetter((x$1: Char)Boolean)")
  def isLetter(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLowerCase((x$1: Char)Boolean)")
  def isLowerCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLowSurrogate((x$1: Char)Boolean)")
  def isLowSurrogate(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isMirrored((x$1: Char)Boolean)")
  def isMirrored(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isSpaceChar((x$1: Char)Boolean)")
  def isSpaceChar(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isTitleCase((x$1: Char)Boolean)")
  def isTitleCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUnicodeIdentifierPart((x$1: Char)Boolean)")
  def isUnicodeIdentifierPart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUnicodeIdentifierStart((x$1: Char)Boolean)")
  def isUnicodeIdentifierStart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUpperCase((x$1: Char)Boolean)")
  def isUpperCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isWhitespace((x$1: Char)Boolean)")
  def isWhitespace(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.reverseBytes((x$1: Char)Char)")
  def reverseBytes(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toLowerCase((x$1: Char)Char)")
  def toLowerCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Character.toTitleCase((x$1: Char)Char)")
  def toTitleCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toUpperCase((x$1: Char)Char)")
  def toUpperCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.valueOf((x$1: Char)java.lang.Character)")
  def valueOf(x1: Char): java.lang.Character = {
    new java.lang.Character('c')
  }
}
