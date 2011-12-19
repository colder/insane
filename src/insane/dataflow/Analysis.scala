package insane
package dataflow

import CFG._
import utils._

class Analysis[E <: EnvAbs[E], S, C <: ControlFlowGraph[S]] (lattice : LatticeAbs[E], baseEnv : E, settings: Settings, initCFG: C) {
  type Vertex = CFGVertex

  object RestartRequest extends Exception;

  var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(lattice.bottom)

  var cfg         = initCFG

  var components = Set[SCC[Vertex]]()
  var topSorted  = Seq[SCC[Vertex]]()
  var analyzed   = Set[SCC[Vertex]]()
  var toAnalyze  = topSorted

  def init() {
    reinit()
    analyzed   = Set()
    facts     += cfg.entry -> baseEnv
  }

  def reinit() {
    var sccs   = new StronglyConnectedComponents(cfg.graph)
    components = sccs.getComponents
    topSorted  = sccs.topSort(components)
    toAnalyze  = topSorted
  }

  def restartWithCFG(cfg: C) {
    this.cfg     = cfg
    reinit()
    throw RestartRequest
  }

  def pass(transferFun: TransferFunctionAbs[E,S]) {
    for (scc <- topSorted; v <- scc.vertices; e <- cfg.graph.inEdges(v)) {
      transferFun(e, facts(e.v1), scc)
    }
  }

  def computeFixpoint(transferFun: TransferFunctionAbs[E,S]) {

    if (settings.displayFullProgress) {
      println("    * Analyzing CFG ("+cfg.graph.V.size+" vertices, "+cfg.graph.E.size+" edges)")
    }

    while (!toAnalyze.isEmpty) {
      try {
        for (scc <- toAnalyze) {
          try {
            computeSCCFixpoint(scc, transferFun)

            analyzed  += scc
            toAnalyze = toAnalyze.tail
          } catch {
            case rr @ RestartRequest =>
              facts --= scc.vertices
              throw rr
          }
        }
      } catch {
        case RestartRequest =>
        if (settings.displayFullProgress) {
          println("    * Re-Analyzing CFG ("+cfg.graph.V.size+" vertices, "+cfg.graph.E.size+" edges)")
        }
        toAnalyze = toAnalyze.filter(!analyzed(_))
      }
    }
  }

  def computeSCCFixpoint(scc: SCC[Vertex], transferFun: TransferFunctionAbs[E,S]) {
    var workList  = scc.vertices

    while (!workList.isEmpty) {
      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- cfg.graph.inEdges(v) if (facts(e.v1) != lattice.bottom || e.v1 == cfg.entry)) {
        val propagated = transferFun(e, facts(e.v1), scc);

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
          val j = lattice.join(nf, oldFact)
          println("Not monotonous!")
          println(" Was:  "+oldFact)
          println("######################")
          println(" Now:  "+nf)
          println("######################")
          println(" Join: "+j)

          println(" Edges:")

          for (e <- cfg.graph.inEdges(v) if (facts(e.v1) != lattice.bottom || e.v1 == cfg.entry)) {
            println("  ** EDGE: "+e.label)
            println("   pre   : => "+facts(e.v1))
            println("   post  : => "+transferFun(e, facts(e.v1), scc))

          }

          sys.error("Abstract Interpretation Fatal Error")
        }
      }

      if (nf != oldFact) {
        facts += v -> nf

        for (v <- cfg.graph.outEdges(v).map(_.v2) & scc.vertices) {
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
