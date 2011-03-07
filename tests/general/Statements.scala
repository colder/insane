package tests

class Statements {
  def a(v1: Int, v2: Int) = {

      val v3 = v1 + 1

      val v4 = 2 + v2

      val v5 = if (v2 > v1 && v2 > 1 || v1 > 3 && v2 > 2) v2 else v1

      def b(a: Int)(b: Int) {

      }

      val c = b(2)_

      val d = c(3)

      val e= b(2)(4);


      var f = 2;
      f = 3;
  }
}
