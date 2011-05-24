package insane
package utils

final class Fraction(_num: Int, _denom: Int) {
  private[this] val g = {
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    gcd(_num max _denom, _num min _denom)
  }

  val num: Int   = _num / g
  val denom: Int = _denom / g

  override def toString = num+"/"+denom

  def * (that: Fraction): Fraction = {
    Fraction(this.num*that.num, this.denom*that.denom)
  }

  def - (that: Fraction): Fraction = {
    Fraction(this.num*that.denom-that.num*this.denom, this.denom*that.denom)
  }

  def + (that: Fraction): Fraction = {
    Fraction(this.num*that.denom+that.num*this.denom, this.denom*that.denom)
  }

  def splitBy(by: Int): Fraction = {
    Fraction(num, denom * by)
  }

  override def equals(o: Any) = o match {
    case that: Fraction =>
      this.num == that.num && this.denom == that.denom
    case i: Int =>
      this == Fraction(i, 1)
    case _ =>
      false
  }

  override def hashCode = (num, denom).hashCode
}

object Fraction {
  def apply(num: Int, denom: Int) = new Fraction(num, denom)

  val zero = new Fraction(0,1)
  val one  = new Fraction(1,1)
  val half = new Fraction(1,2)
}
