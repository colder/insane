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

  /**
   * Functions repository
   * we make a clear distinction between methods declared in analyzed source
   * code, and methods extracted from binaries or other means.
   */
  private var declaredFunctions_  = Map[Symbol, AbsFunction]()
  private var extractedFunctions_ = Map[Symbol, AbsFunction]()

  def registerDeclaredFunction(sym: Symbol, fun: AbsFunction) {
    declaredFunctions_ += sym -> fun
  }

  def registerExtractedFunction(sym: Symbol, fun: AbsFunction) {
    extractedFunctions_ += sym -> fun
  }

  def hasFunction(sym: Symbol): Boolean = {
    declaredFunctions_.contains(sym) || extractedFunctions_.contains(sym)
  }

  def lookupFunction(sym: Symbol): Option[AbsFunction] = {
    declaredFunctions_.get(sym)
      .orElse(extractedFunctions_.get(sym))
      .orElse(ICodeFunction.fromSymbol(sym))
  }

  def declaredFunctions  = declaredFunctions_
  def extractedFunctions = extractedFunctions_
  def allFunctions       = declaredFunctions_ ++ extractedFunctions_

  // Contains a mapping from method stubs and their corresponding 'fake' implementation
  var methodProxies        = Map[Symbol, AbsFunction]()

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
  var preciseCallTargetsCache = Map[CFG.AssignApplyMeth, Set[(FunctionCFG, DualTypeMap)]]()

  // Some information about the current state of the analysis
  var analysisStackSet        = Set[(Symbol, TypeSignature)]()
  var recursiveMethods        = Set[(Symbol, TypeSignature)]()

  class AnalysisContext(
    val cfg: FunctionCFG,
    val sig: TypeSignature,
    val mode: AnalysisMode) {
  }

  var analysisStack                   = Stack[AnalysisContext]()
  var currentContext: AnalysisContext = null


  def dumpAnalysisStack() {
    println(" ** Dumping Analysis Stacktrace: **")
    for ((frame, i) <- (analysisStack.zipWithIndex)) {
      println("  "+i+": "+frame.cfg.symbol.fullName+" ["+frame.mode+"] "+frame.sig+"")
      new CFGDotConverter(frame.cfg, "CFG of "+frame.cfg.symbol.fullName).writeFile("frame"+i+".dot")
    }
  }

  lazy val debugOutput = new OutputHandlers.Debug
  def displayAnalysisContext() {
    debugOutput.print("\033[2J\033[1;1H"); // Clear screen

    def o(str: String) {
      debugOutput.println(str)
    }

    o("Detected as recursive: "+recursiveMethods.map{ case (sym, sig) => sym.fullName+"["+sig+"]" }.toSet.mkString(", ")+"\n")

    if (analysisStack.size > 0) {
      var prefix = ""
      for(ac <- analysisStack.pop.reverseIterator) {
        o(prefix+ac.cfg.symbol.fullName+" ["+ac.mode+"] "+ac.sig+"")
        if (prefix == "") {
          prefix = " └ "
        } else {
          prefix = "  " +prefix
        }
      }

      val top = analysisStack.top;
      o(prefix+Console.BOLD+top.cfg.symbol.fullName+" ["+top.mode+"] "+top.sig+Console.RESET)
    } else {
      o("Done.");
    }
  }
}
