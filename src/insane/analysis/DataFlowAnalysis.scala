package insane
package analysis

import CFG._
import utils._

class DataFlowAnalysis[E <: DataFlowEnvAbs[E, S], S] (bottomEnv : E, baseEnv : E, settings: Settings) {
  type Vertex = CFGVertex[S]

  var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(bottomEnv)

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
      if (cfg.inEdges(v).forall(e => (facts(e.v1) != bottomEnv) &&
                                     (transferFun(e.label, facts(e.v1)) == bottomEnv))) {

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
      var newFact : Option[E] = None

      for (e <- cfg.inEdges(v) if facts(e.v1) != bottomEnv) {
        val propagated = transferFun(e.label, facts(e.v1));

        if (propagated != bottomEnv) {
          newFact = newFact match {
            case Some(nf) => Some(nf union propagated)
            case None => Some(propagated)
          }
        }
      }

      val nf = newFact.getOrElse(oldFact.copy);

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
