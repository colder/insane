trait Parametrized[A] {
  def id(a: A): A = a
}

class Class1 extends Parametrized[Class1] {
  def plop() = 2
}

object Test {
  val c1 = new Class1
  val c2 = new Class1

  val c3 = c1.id(c2)

  c3.plop()
}
