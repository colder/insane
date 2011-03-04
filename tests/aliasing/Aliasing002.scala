package tests.aliasing
class A {

}

class Aliasing002(a: A) {
  var c = new A

  var b = if (2>3) c else a

  def test(a1: A, a2: A) = {
    require(a1 ne a2)
  }

  test(a, b)
}
