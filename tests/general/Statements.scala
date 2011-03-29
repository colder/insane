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

      var myint = 3

      myint match {
        case 1 =>
          myint += 1
        case 2 =>
          myint += 2
        case 3 =>
          myint += 3
      }

      val cla = new Statements;

      cla.asInstanceOf[AnyRef].eq(this);
      val plop = cla.isInstanceOf[AnyRef];

      try {
        myint += 10
      } finally {
        myint += 12
      }
  }

  def b = {
    def a(i: Int): Int = {
      if (i > 4) {
        i
      } else {
        a(i+2)
      }
    }
  }

  def c = {
    Test.testmethod(3)
  }

  def d(asd: String) = {
    "foobar" + asd
  }

  def e(a: Any) = {
    import Extractors._
    var myvar = 2;
    myvar match {
      case ExPlop(a) => 3
      case ExPlip(b) => 4
    }

  }

  def ff = {
    import scala.collection.immutable.ListSet
    var ls = new ListSet[Int]()
    ls += 2
    ls += 4

    ls.toList.mkString(", ")

    TestObject.a+1
  }
}

object Test {
  def testmethod(i: Int) = 2+i
}

object Extractors {
  object ExPlop {
    def unapply(a: Any): Option[Int] = Some(2)
  }
  object ExPlip {
    def unapply(a: Any): Option[Int] = Some(3)
  }
}

object TestObject {
  val a = 2;
}
