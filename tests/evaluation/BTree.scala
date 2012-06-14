package btree

sealed abstract class AbsNode {
  def compute: Int
}

class Node(val right: AbsNode, val left: AbsNode) extends AbsNode {
  var cache = -1;

  def compute = {
    if (cache < 0) {
      cache = right.compute + left.compute
    }
    cache
  }
}

class Leaf(value: Int) extends AbsNode {
  def compute = value
}


object Test {

  def parSum(n: Node) = {
    doSum1(n) + doSum2(n)
  }

  def doSum1(n: Node) = {
    n.right.compute
  }
  def doSum2(n: Node) = {
    n.left.compute
  }
}
