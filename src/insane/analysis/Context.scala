package insane
package analysis

import utils.SCC


trait Context extends Functions {
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
  var callGraphSCCs          = Seq[SCC[CAVertex]]()


  val classDescendentGraph   = new ClassDescendentGraph

  // Contains, for every possible Apply, the list of symbols that it potentially points to, and whether it is exhaustive
  var callTargets           = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()
  // Stores the result of point-to analysis for each method symbol
  var pointToEnvs           = Map[Symbol, PTEnv]()
  // Stores results of purity information
  var purityResults         = Map[Symbol, PurityInfo]()
}
