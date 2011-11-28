package insane
package dataflow

import CFG._
import utils._

class Analysis[E <: EnvAbs[E], S, C <: ControlFlowGraph[S]] (lattice : LatticeAbs[E], baseEnv : E, settings: Settings, var cfg: C) {
  type Vertex = CFGVertex

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
    var sccs   = new StronglyConnectedComponents(cfg.graph)
    components = sccs.getComponents
    topSorted  = sccs.topSort(components)
    toAnalyse  = topSorted
  }

  def pass(func: (S, E) => Unit) {
    for (v <- cfg.graph.V) {
      for (e <- cfg.graph.inEdges(v)) {
          func(e.label, facts(e.v1))
      }
    }
  }


  // Unused ?
  private def detectUnreachable(transferFun: TransferFunctionAbs[E,S]): List[S] = {
    var res : List[S] = Nil;

    for (v <- cfg.graph.V if v != cfg.entry) {
      if (cfg.graph.inEdges(v).forall(e => (facts(e.v1) != lattice.bottom) &&
                                     (transferFun(e, facts(e.v1)) == lattice.bottom))) {

        for (e <- cfg.graph.outEdges(v)) {
          res = e.label :: res
        }
      }
    }

    res
  }

  var forceRestart      = false

  def restartWithCFG(cfg: C) {
    this.cfg          = cfg
    forceRestart      = true
    reinit()
  }

  def computeFixpoint(transferFun: TransferFunctionAbs[E,S]) {

    if (settings.displayFullProgress) {
      println("    * Analyzing CFG ("+cfg.graph.V.size+" vertices, "+cfg.graph.E.size+" edges)")
    }

    while (!toAnalyse.isEmpty) {
      for (scc <- toAnalyse if !forceRestart) {
        computeSCCFixpoint(scc, transferFun)

        if (!forceRestart) {
          analyzed  += scc
          toAnalyse = toAnalyse.tail
        } else {
          facts --= scc.vertices
        }
      }

      if (forceRestart) {
        forceRestart      = false
        if (settings.displayFullProgress) {
          println("    * Re-Analyzing CFG ("+cfg.graph.V.size+" vertices, "+cfg.graph.E.size+" edges)")
        }
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

      if (pass > 50000) sys.error("Terminating, looks endless...")

      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- currentCFG.graph.inEdges(v) if (facts(e.v1) != lattice.bottom) && !forceRestart) {
        val propagated = transferFun(e, facts(e.v1));

        if (propagated != lattice.bottom) {
          newFacts = propagated :: newFacts
        }
      }

      val nf = if (newFacts.isEmpty) {
        oldFact.duplicate
      } else {
        lattice.join(newFacts : _*)
      }

      settings.ifDebug {
        if (lattice.join(nf, oldFact) != nf) {
          println("Not monotonous!")
          println(" Was: "+oldFact)
          println("######################")
          println(" Now: "+nf)
          println("######################")
          println(" Edges:")

          for (e <- currentCFG.graph.inEdges(v) if (facts(e.v1) != lattice.bottom)) {
            println("  ** EDGE: "+e.label)
            println("   pre   : => "+facts(e.v1))
            println("   post  : => "+transferFun(e, facts(e.v1)))

          }

          sys.error("Abstract Interpretation Fatal Error")
        }
      }

      if (nf != oldFact) {
        facts += v -> nf

        for (v <- currentCFG.graph.outEdges(v).map(_.v2) & scc.vertices) {
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


  init()
}
