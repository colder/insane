package insane
package utils

import Graphs._

class SCC[Vertex <: VertexAbs[_ <: EdgeAbs[Vertex]]](val vs: Set[Vertex], var outSCC: Set[SCC[Vertex]]) {
  override def toString = {
    vs.mkString("[", ", ", "]")
  }
}

class StronglyConnectedComponents[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]](cfg: DirectedGraph[Vertex, Edge]) {

  type mySCC = SCC[Vertex]

  class Node(var v: Vertex, var index: Int, var vindex: Int, var lowlink: Int, var caller: Option[Node], var vSeq: IndexedSeq[Vertex]) {
    override def toString = v.toString
  }

  def getComponents = {
    var sccs     = Set[mySCC]()
    var vToScc   = Map[Vertex, mySCC]()

    var onStack  = Set[Node]()
    var stack    = List[Node]()
    var index    = 0

    var nodes    = Map[Vertex, Node]()

    def push(n: Node) = {
      stack = n :: stack
      onStack += n
    }

    def pop: Node = {
      val n = stack.head
      stack = stack.tail
      onStack -= n
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
          } else if (onStack contains optN.get) {
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
      scc.outSCC = scc.vs.flatMap(v => cfg.outEdges(v).map(e => vToScc(e.v2)).filter(_ != scc)).toSet
    }

    sccs
  }

  def topSort(sccs: Set[mySCC]): Seq[mySCC] = {
    // first we get the roots
    var todo = (sccs &~ sccs.flatMap(scc => scc.outSCC)).toSeq

    var res = Seq[mySCC]()

    while (todo.size > 0) {
      val scc = todo.head
      todo = todo.tail

      res :+= scc

      for (s <- scc.outSCC) {
        todo = todo :+ s
      }
    }

    res
  }
}


