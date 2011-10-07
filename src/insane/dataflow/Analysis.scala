package insane
package dataflow

import CFG._
import utils._

class Analysis[E <: EnvAbs[E, S], S] (lattice : LatticeAbs[E, S], baseEnv : E, settings: Settings) {
  type Vertex = CFGVertex[S]

  assert(lattice.bottom != baseEnv, "The analysis will not be done if bottomEnv is == baseEnv")

  var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(lattice.bottom)

    def pass(cfg: ControlFlowGraph[S], func: (S, E) => Unit) {
    for (v <- cfg.V) {
      for (e <- cfg.inEdges(v)) {
          func(e.label, facts(e.v1))
      }
    }
  }


  def detectUnreachable(cfg: ControlFlowGraph[S], transferFun: TransferFunctionAbs[E,S]): List[S] = {
    var res : List[S] = Nil;

    for (v <- cfg.V if v != cfg.entry) {
      if (cfg.inEdges(v).forall(e => (facts(e.v1) != lattice.bottom) &&
                                     (transferFun(e.label, facts(e.v1)) == lattice.bottom))) {

        for (e <- cfg.outEdges(v)) {
          res = e.label :: res
        }
      }
    }

    res
  }

  def computeFixpoint(cfg: ControlFlowGraph[S], transferFun: TransferFunctionAbs[E,S]) {
    if (settings.displayFullProgress) {
      println("    * Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")
    }

    facts += cfg.entry -> baseEnv

    val scc        = new StronglyConnectedComponents(cfg)
    val components = scc.getComponents

    for (scc <- scc.topSort(components)) {
      computeFixpointSSC(scc, cfg, transferFun)
    }
  }

  def computeFixpointSSC(scc: SCC[Vertex], cfg: ControlFlowGraph[S], transferFun: TransferFunctionAbs[E,S]) {
    var pass = 0;

    var workList  = scc.vertices;

    while (workList.size > 0) {
      pass += 1

      if (settings.extensiveDebug) {
        println("    * Pass "+pass+" ("+workList.size+" nodes in worklist)...")
      }

      if (pass > 10000) sys.error("Terminating, looks endless...")

      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- cfg.inEdges(v) if facts(e.v1) != lattice.bottom) {
        val propagated = transferFun(e.label, facts(e.v1));

        if (propagated != lattice.bottom) {
          newFacts = propagated :: newFacts
        }
      }

      val nf = if (newFacts.isEmpty) {
        oldFact.duplicate
      } else {
        lattice.join(newFacts : _*)
      }

      if (nf != oldFact) {
        facts += v -> nf

        for (v <- cfg.outEdges(v).map(_.v2) & scc.vertices) {
          workList += v;
        }
      }
    }
  }

  def dumpFacts {
    for ((v,e) <- facts.toList.sortWith{(x,y) => x._1.name < y._1.name}) {
      println("  "+v+" => "+e)
    }
  }

  def getResult : Map[Vertex,E] = facts
}
