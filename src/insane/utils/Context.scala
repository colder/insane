package insane
package utils

import CFG._


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

  var analysisStack         = Set[Symbol]()

  val classHierarchyGraph   = new ClassHierarchyGraph

  var methodCallsStats      = Map[UniqueID, (Int, Int)]()

  // Stores targets when doing precise fixpoint, will been used when reducing
  // to avoid precision loss while keeping soundness
  var preciseCallTargetsCache = Map[CFG.AssignApplyMeth, Set[FunctionCFG]]()
}
