package btree

class Data(var i: Int)

class BinaryTree(val head: AbsNode) {

  def search(d: Data): Boolean = {
    head.search(d)
  }
}

abstract sealed class AbsNode {
  def search(d: Data): Boolean
}

object Leaf extends AbsNode {
  def search(d: Data): Boolean = false
}

class Node(data: Data, left: AbsNode, right: AbsNode) extends AbsNode {
  def search(d: Data): Boolean = {
    d == data || left.search(d) || right.search(d)
  }
}

object Usage {
  def newbTree = {
    new BinaryTree(new Node(new Data(0), new Node(new Data(1), Leaf, Leaf), new Node(new Data(2), Leaf, Leaf)))
  }
  def use = {
    newbTree.search(new Data(2))
  }
}
