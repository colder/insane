package tests

class Test1 {
  def a(v1: Test1, v2: Test1) = {
    require(v1 ne v2)

    v1

  } ensuring (r => r eq v1)
}
