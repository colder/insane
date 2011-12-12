package insane
package dataflow

import CFG._
import utils._

class Analysis[E <: EnvAbs[E], S, C <: ControlFlowGraph[S]] (lattice : LatticeAbs[E], baseEnv : E, settings: Settings, initCFG: C) {
  type Vertex = CFGVertex


  object RestartRequest extends Exception;

  var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(lattice.bottom)

  var cfg         = initCFG
  var metadata: Metadata[E,S,C] = null

  def setMetadata(m: Metadata[E,S,C]) {
    metadata = m
  }

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

  def restartWithCFG(cfg: C) {
    this.cfg     = cfg
    reinit()
    throw RestartRequest
  }

  def pass(func: TransferFunctionAbs[E,S]) {
    for (v <- cfg.graph.V) {
      for (e <- cfg.graph.inEdges(v)) {
          func(e, facts(e.v1), metadata)
      }
    }
  }

  def computeFixpoint(transferFun: TransferFunctionAbs[E,S]) {

    if (settings.displayFullProgress) {
      println("    * Analyzing CFG ("+cfg.graph.V.size+" vertices, "+cfg.graph.E.size+" edges)")
    }

    while (!toAnalyse.isEmpty) {
      try {
        for (scc <- toAnalyse) {
          try {
            computeSCCFixpoint(scc, transferFun)

            analyzed  += scc
            toAnalyse = toAnalyse.tail
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
        toAnalyse = toAnalyse.filter(!analyzed(_))
      }
    }
  }

  def computeSCCFixpoint(scc: SCC[Vertex], transferFun: TransferFunctionAbs[E,S]) {
    var pass = 0;

    var workList  = scc.vertices

    while (!workList.isEmpty) {
      pass += 1

      val v = workList.head
      workList -= v

      val oldFact : E = facts(v)
      var newFacts = List[E]()

      for (e <- cfg.graph.inEdges(v) if (facts(e.v1) != lattice.bottom)) {
        val propagated = transferFun(e, facts(e.v1), metadata);

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

          for (e <- cfg.graph.inEdges(v) if (facts(e.v1) != lattice.bottom)) {
            println("  ** EDGE: "+e.label)
            println("   pre   : => "+facts(e.v1))
            println("   post  : => "+transferFun(e, facts(e.v1), metadata))

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
