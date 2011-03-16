package tests.general

class Statements {
  private[this] var f = 2;
  def getThis(i: Int) = this
  def a(v1: Int, v2: Int) = {

      val v3 = v1 + 1

      val test = getThis(1).getThis(2).getThis(3)

      val v4 = 2 + v2

      val v5 = if (v2 > v1 && v2 > 1 || v1 > 3 && v2 > 2) v2 else v1

      def b(a: Int)(b: Int) {

      }

      val c = b(2)_

      val d = c(3)

      val e= b(2)(4);

      f = 3;
      f = 4;

      for(f2 <- List(1,2,3)) {
        f = f2
      }

      while(f < 4) {
        f += 1
      }


  }
}
