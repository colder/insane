package t2

class Data(var d: Boolean)

class P {
  var x: Int = 0
  var y: Int = 0
}

class C1(var c1: Data) extends P
class C2(var c2: Data) extends P

object Usage {

  def plopWrite(o: P) = {
    if (o.isInstanceOf[C1]) {
      o.asInstanceOf[C1].c1.d = true
      o.x = 2;
    } else {
      o.asInstanceOf[C2].c2.d = false
      o.y = 2;
    }
  }

  def plopUseWrite() = {
    val myC2 = new C2(new Data(true))

    plopWrite(myC2)

    if (!myC2.c2.d) {
      myC2.c2 = null;
    }

    myC2
  }
}
