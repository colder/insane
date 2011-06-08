package insane
package utils

import Graphs._

class SCC[Vertex <: VertexAbs[_ <: EdgeAbs[Vertex]]](val vertices: Set[Vertex], var outSCC: Set[SCC[Vertex]]) {
  // Used by topsort
  var inQueue = false

  override def toString = {
    vertices.mkString("[", ", ", "]")
  }
}

class StronglyConnectedComponents[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]](cfg: DirectedGraph[Vertex, Edge]) {

  type mySCC = SCC[Vertex]

  class Node(var v: Vertex, var index: Int, var vindex: Int, var lowlink: Int, var caller: Option[Node], var vSeq: IndexedSeq[Vertex]) {
    override def toString = v.toString
    var onStack = false
  }

  def getComponents = {
    var sccs     = Set[mySCC]()
    var vToScc   = Map[Vertex, mySCC]()

    var stack    = new collection.mutable.Stack[Node]()
    var index    = 0

    var nodes    = Map[Vertex, Node]()

    def push(n: Node) = {
      stack push n
      n.onStack = true
    }

    def pop: Node = {
      val n = stack.pop
      n.onStack = false
      n
    }

    def tarjan(v: Vertex) {
      val n1= new Node(v, index, 0, index, None, cfg.outEdges(v).toIndexedSeq.map((e: Edge) => e.v2))
      nodes += v -> n1
      index += 1
      push(n1)

      var last = n1
      var continue = true

      while (continue) {
        if (last.vindex < last.vSeq.size) {
          val nv = last.vSeq(last.vindex)
          last.vindex += 1;

          val optN = nodes.get(nv)
          if (optN == None) {
            val n2 = new Node(nv, index, 0, index, Some(last), cfg.outEdges(nv).toIndexedSeq.map((e: Edge) => e.v2))
            nodes += nv -> n2
            index += 1
            push(n2)
            last = n2
          } else if (optN.get.onStack) {
            last.lowlink = last.lowlink.min(optN.get.index)
          }

        } else {
          if (last.lowlink == last.index) {
            // SCC
            var set = Set[Vertex]()
            var top = stack.head
            set += pop.v

            while(top != last) {
              top = pop
              set += top.v
            }
            val scc = new mySCC(set, Set())

            set.foreach(vToScc += _ -> scc)

            sccs += scc
          }

          val optCaller = last.caller
          if (optCaller == None) {
            continue = false
          } else {
            optCaller.get.lowlink = optCaller.get.lowlink.min(last.lowlink)
            last = optCaller.get
          }
        }
      }
    }


    var vertices = cfg.V.toSeq
    while(!vertices.isEmpty) {
      val node = vertices.head
      vertices = vertices.tail

      if (!(nodes contains node)) {
        tarjan(node)
      }
    }

    // be blunt, traverse again to get adjacent SCC
    for(scc <- sccs) {
      scc.outSCC = scc.vertices.flatMap(v => cfg.outEdges(v).map(e => vToScc(e.v2)).filter(_ != scc)).toSet
    }

    sccs
  }

  def topSort(sccs: Set[mySCC]): Seq[mySCC] = {
    import collection.mutable.Queue

    // first we get the roots
    var todo = new Queue[mySCC]()
    todo ++= sccs &~ sccs.flatMap(scc => scc.outSCC)

    todo.foreach {
      s => s.inQueue = true
    }

    var res = Seq[mySCC]()

    while (!todo.isEmpty) {
      val scc = todo.dequeue()

      scc.inQueue = false

      res = scc +: res

      for (s <- scc.outSCC if !s.inQueue) {
        s.inQueue = true
        todo += s
      }
    }

    res.reverse
  }
}


