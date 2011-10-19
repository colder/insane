package insane
package dataflow

import CFG._
import utils._

class Analysis[E <: EnvAbs[E, S], S] (lattice : LatticeAbs[E, S], baseEnv : E, settings: Settings, var cfg: ControlFlowGraph[S]) {
  type Vertex = CFGVertex[S]

  var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(lattice.bottom)

  var components = Set[SCC[Vertex]]()
  var topSorted  = Seq[SCC[Vertex]]()
  var analyzed   = Set[SCC[Vertex]]()
  var toAnalyse  = topSorted

  def init() {
    reinit()
    analyzed   = Set()
    facts     += cfg.entry -> baseEnv
  }

  def reinit() {
    var sccs   = new StronglyConnectedComponents(cfg)
    components = sccs.getComponents
    topSorted  = sccs.topSort(components)
    toAnalyse  = topSorted
  }

  def pass(func: (S, E) => Unit) {
    for (v <- cfg.V) {
      for (e <- cfg.inEdges(v)) {
          func(e.label, facts(e.v1))
      }
    }
  }


  // Unused ?
  def detectUnreachable(transferFun: TransferFunctionAbs[E,S]): List[S] = {
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

  var forceRestart      = false

  def restartWithCFG(cfg: ControlFlowGraph[S]) {
    this.cfg          = cfg
    forceRestart      = true
    reinit()
  }

  def computeFixpoint(transferFun: TransferFunctionAbs[E,S]) {

    println("    * Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")

    while (!toAnalyse.isEmpty) {
      for (scc <- toAnalyse if !forceRestart) {
        computeSCCFixpoint(scc, transferFun)

        if (!forceRestart) {
          analyzed += scc
        } else {
          facts --= scc.vertices
        }
      }

      if (forceRestart) {
        forceRestart      = false
        println("    * Re-Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")
        toAnalyse = toAnalyse.filter(!analyzed(_))
      }
    }
  }

  def computeSCCFixpoint(scc: SCC[Vertex], transferFun: TransferFunctionAbs[E,S]) {
    var pass = 0;

    var currentCFG = cfg

    var workList  = scc.vertices

    while (!workList.isEmpty && !forceRestart) {
      pass += 1

      if (settings.extensiveDebug) {
        println("    * Pass "+pass+" ("+workList.size+" nodes in worklist)...")
      }

      if (pass > 10000) sys.error("Terminating, looks endless...")

      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- currentCFG.inEdges(v) if (facts(e.v1) != lattice.bottom || e.v1 == cfg.entry) && !forceRestart) {
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

        for (v <- currentCFG.outEdges(v).map(_.v2) & scc.vertices) {
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
