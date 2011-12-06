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

  // contains types calculated statically during type analsys for each method call statements
  var methodCallsStaticTypes = Map[UniqueID, ObjectSet]()

  val classHierarchyGraph   = new ClassHierarchyGraph

  var methodCallsStats      = Map[UniqueID, (Int, Int)]()
}
