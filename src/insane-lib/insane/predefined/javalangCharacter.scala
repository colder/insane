package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Character")
class javalangCharacter {
  @AbstractsMethod("java.lang.Character.charValue(()Char)")
  def __charValue(): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.digit((x$1: Char, x$2: Int)Int)")
  def __digit(x1: Char, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getDirectionality((x$1: Char)Byte)")
  def __getDirectionality(x1: Char): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Character.getNumericValue((x$1: Char)Int)")
  def __getNumericValue(x1: Char): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getType((x$1: Char)Int)")
  def __getType(x1: Char): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.getType((x$1: Int)Int)")
  def __getType(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Character.<init>((x$1: Char)java.lang.Character)")
  def ____init__(x1: Char): javalangCharacter = {
    this
  }
  @AbstractsMethod("java.lang.Character.isDigit((x$1: Char)Boolean)")
  def __isDigit(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isHighSurrogate((x$1: Char)Boolean)")
  def __isHighSurrogate(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isIdentifierIgnorable((x$1: Char)Boolean)")
  def __isIdentifierIgnorable(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isISOControl((x$1: Char)Boolean)")
  def __isISOControl(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isJavaIdentifierPart((x$1: Char)Boolean)")
  def __isJavaIdentifierPart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLetterOrDigit((x$1: Char)Boolean)")
  def __isLetterOrDigit(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLetter((x$1: Char)Boolean)")
  def __isLetter(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLowerCase((x$1: Char)Boolean)")
  def __isLowerCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isLowSurrogate((x$1: Char)Boolean)")
  def __isLowSurrogate(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isMirrored((x$1: Char)Boolean)")
  def __isMirrored(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isSpaceChar((x$1: Char)Boolean)")
  def __isSpaceChar(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isTitleCase((x$1: Char)Boolean)")
  def __isTitleCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUnicodeIdentifierPart((x$1: Char)Boolean)")
  def __isUnicodeIdentifierPart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUnicodeIdentifierStart((x$1: Char)Boolean)")
  def __isUnicodeIdentifierStart(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isUpperCase((x$1: Char)Boolean)")
  def __isUpperCase(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.isWhitespace((x$1: Char)Boolean)")
  def __isWhitespace(x1: Char): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Character.reverseBytes((x$1: Char)Char)")
  def __reverseBytes(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toLowerCase((x$1: Char)Char)")
  def __toLowerCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Character.toTitleCase((x$1: Char)Char)")
  def __toTitleCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.toUpperCase((x$1: Char)Char)")
  def __toUpperCase(x1: Char): Char = {
    'c'
  }
  @AbstractsMethod("java.lang.Character.valueOf((x$1: Char)java.lang.Character)")
  def __valueOf(x1: Char): java.lang.Character = {
    new java.lang.Character('c')
  }
}
