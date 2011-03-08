package insane

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import AST.Extractors

trait CodeExtraction extends Extractors {
  self: AnalysisComponent =>

  import global._
  import global.definitions._

  import StructuralExtractors._

  def extractConditions(unit: CompilationUnit): Unit = {
    new ForeachTreeTraverser(step).traverse(unit.body)
  }

  def step(tree: Tree): Unit = tree match {
      case DefDef(_, name, _, _, _, rhs) =>
        extractFunBody(rhs)
      case _ =>
  }

  def extractFunBody(body: Tree): Unit = {
    var realBody = body

    realBody match {
      case ExEnsuredExpression(innerBody, resSym, contract) =>
        realBody = innerBody
        println("Ensures: "+contract)

      case _ =>
    }

    var continue = true
    while(continue) {
      realBody match {
        case ExRequiredExpression(innerBody, contract) =>
          realBody = innerBody
          println("Requires: "+contract)

        case _ =>
          continue = false
      }
    }
  }
}
