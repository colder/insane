package t6

class Data(var b: Boolean);

final class SimpleList(val head: Data, val tail: SimpleList)


object Usage {

  def create = {
    new SimpleList(new Data(true), new SimpleList(new Data(true), new SimpleList(new Data(true), null)))
  }

  def use(l: SimpleList) = {
    var tmp = l
    while(tmp != null && tmp.tail != null) {
      tmp = tmp.tail
    }

    tmp.head.b = false;
  }
}
