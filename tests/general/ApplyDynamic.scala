object foo {
  def test() = {
    class A {
    }

    val a1 = new Object {
      def plop1 = 42
    }

    val a2 = new A {
      def plop2 = 42
    }


    a1.plop1
    a2.plop2
  }
}
