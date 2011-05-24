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
    var pass = 0;

    if (settings.displayFullProgress) {
      println("    * Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")
    }

    facts += cfg.entry -> baseEnv

    var workList  = Set[Vertex]();

    for (e <- cfg.outEdges(cfg.entry)) {
      workList += e.v2
    }

    while (workList.size > 0) {
      pass += 1

      if (settings.displayFullProgress) {
        println("    * Pass "+pass+" ("+workList.size+" nodes in worklist)...")
      }

      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- cfg.inEdges(v) if facts(e.v1) != lattice.bottom) {
        val propagated = transferFun(e.label, facts(e.v1));

        if (propagated != lattice.bottom) {
          newFacts = propagated :: newFacts
          /*
          newFact = newFact match {
            case Some(nf) => Some(nf union propagated)
            case None => Some(propagated)
          }
          */
        }
      }

      /*
      val nf = newFact.getOrElse(oldFact.duplicate);
      */

      val nf = if (newFacts.isEmpty) {
        oldFact.duplicate
      } else {
        lattice.join(newFacts : _*)
      }

      if (nf != oldFact) {
        facts += v -> nf

        for (e <- cfg.outEdges(v)) {
          workList += e.v2;
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
