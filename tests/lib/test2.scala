package t2

class Data(var d: Boolean)

class P;

class C1(var c1: Data) extends P
class C2(var c2: Data) extends P

object Usage {

  def plopWrite(o: P) = {
    if (o.isInstanceOf[C1]) {
      o.asInstanceOf[C1].c1.d = true
    } else {
      o.asInstanceOf[C2].c2.d = false
    }
  }

  def plopUseWrite() = {
    val myC2 = new C2(new Data(true))

    plopWrite(myC2)

    myC2
  }
}
