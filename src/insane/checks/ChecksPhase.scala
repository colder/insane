package insane
package checks

import utils._
import utils.Reporters._
import utils.Automatons._
import utils.RegularExpressions.RegexHelpers._

trait Checks { self: AnalysisComponent =>

  import global._

  class ChecksPhase extends SubPhase {
    val name = "Running Post-Analysis Checks"

    def run() {
      if (!settings.toCheck.isEmpty) {
        val columns = Seq(TableColumn("Function Name", Some(40)),
                          TableColumn("Type", None),
                          TableColumn("Regex", Some(30)),
                          TableColumn("Result", None),
                          TableColumn("Signature", Some(80)))

        val table  = new Table(columns)
        var cntAss = 0;

        val funsToCheck = allFunctions.filter(fun => settings.shouldCheck(safeFullName(fun._1)))

        for ((s, fun) <- funsToCheck.toSeq.sortBy(x => safeFullName(x._1))) {
          for(((sig, res), i) <- fun.flatPTCFGs.zipWithIndex) {
            val effect = res.getFlatEffect

            val dfa: Automaton[String, Int] = new NFAEffectRepresentation(effect).getNFA.mapTransitions(_.toSimpleString).determinize.minimize


            for (contr <- fun.contrEffects) {
              val (tpe, reg, res) = contr match {
                case a @ AssertOnlyModified(regex, region) =>
                  val others = dfa - region
                  if (effect.category.isBottom) {
                    ("Only", regex, "BOTTOM")
                  } else if (effect.category.isTop) {
                    ("Only", regex, "TOP")
                  } else if (others.isImpossible) {
                    ("Only", regex, "PASSED")
                  } else {
                    reporter.warn(List("Assertion OnlyModified("+regex+") failed:",
                      "Effect Obtained: "+nfaToRegex(dfa),
                      "Counter example: "+nfaToRegex(others)))
                    ("Only", regex, "FAILED")
                  }
                case a @ AssertUntouched(regex, region) =>
                  val others = dfa intersection region
                  if (effect.category.isBottom) {
                    ("Not", regex, "BOTTOM")
                  } else if (effect.category.isTop) {
                    ("Not", regex, "TOP")
                  } else if (others.isImpossible) {
                    ("Not", regex, "PASSED")
                  } else {
                    reporter.warn(List("Assertion MayModify("+regex+") failed:",
                      "Effect Obtained: "+nfaToRegex(dfa),
                      "Counter example: "+nfaToRegex(others)))
                    ("Not", regex, "FAILED")
                  }
              }
              cntAss += 1
              table.addRow(TableRow() | fun.symbol.fullName | tpe | reg.toString | res | sig.toString)
            }
          }
        }

        if (cntAss > 0) {
          reporter.dispatch(table.draw _)
        }
        reporter.msg(" -> Checked "+cntAss+" assertions.")
      }
    }
  }
}
