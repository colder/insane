package tests.aliasing
class A {

}

class Aliasing002(a: A) {
  var c = new A

  var b = id(a);

  def test(a1: A, a2: A) = {
    require(a1 ne a2)
  }

  def id(a: A) = a

  test(a, b)
}
