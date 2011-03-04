package tests.aliasing
class A {

}

class Aliasing001(a: A) {
  var c = new A()

  var b = c

  def test(a1: A, a2: A) = {
    require(a1 ne a2)
  }

  test(a, b)
}
