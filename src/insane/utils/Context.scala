package insane
package utils

import CFG._
import collection.immutable.Stack


/**
 * Context contains structures shared between multiple analysis subphases
 */
trait Context {
  self: AnalysisComponent =>

  import global._

  var funDecls = Map[Symbol, AbsFunction]()

  // full callgraph
  val callGraph              = new CallGraph
  // contains a map x -> ys, where ys represents methods called by x
  var simpleCallGraph        = Map[Symbol, Set[Symbol]]().withDefaultValue(Set())
  // contains a map y -> xs, where xs represents methods that call y
  var simpleReverseCallGraph = Map[Symbol, Set[Symbol]]().withDefaultValue(Set())
  // contains the strongly connected components, topologically ordered, of the call graph
  var callGraphSCCs          = Seq[SCC[TAVertex]]()

  var methCallSCC            = Map[Symbol, SCC[TAVertex]]()

  val classHierarchyGraph   = new ClassHierarchyGraph

  var methodCallsStats      = Map[UniqueID, (Int, Int)]()

  // Stores targets when doing precise fixpoint, will been used when reducing
  // to avoid precision loss while keeping soundness
  var preciseCallTargetsCache = Map[CFG.AssignApplyMeth, Set[FunctionCFG]]()

  // Some information about the current state of the analysis
  var analysisStackSet        = Set[Symbol]()
  var analysisStack           = Stack[(Symbol, Seq[ObjectSet], AnalysisMode)]()
  var currentCFG: FunctionCFG = null;

  var recursiveMethods        = Set[(Symbol, Seq[ObjectSet])]()

  def displayAnalysisContext() {
    reporter.debug.print("\033[2J\033[1;1H"); // Clear screen

    def o(str: String) {
      reporter.debug.println(str)
    }

    if (currentCFG != null) {
      o("Current CFG has "+currentCFG.graph.V.size+" nodes\n")
    }

    o("Detected as recursive: "+recursiveMethods.map(_._1.fullName).toSet.mkString(", ")+"\n")

    if (analysisStack.size > 0) {
      var prefix = ""
      for((sym, callargs, mode) <- analysisStack.pop.reverseIterator) {
        o(prefix+sym.fullName+" ["+mode+"] ")
        if (prefix == "") {
          prefix = " └ "
        } else {
          prefix = "  " +prefix
        }
      }

      o(prefix+Console.BOLD+analysisStack.top._1.fullName+" ["+analysisStack.top._3+"]"+Console.RESET)
    } else {
      o("Done.");
    }
  }
}
