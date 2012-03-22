package scala
abstract class Function1[-T1, +R] {
  def apply(a1: T1): R
}

abstract class Function2[-T1, -T2, +R] {
  def apply(a1: T1, a2: T2): R
}
