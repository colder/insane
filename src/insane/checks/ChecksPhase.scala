package insane
package checks

import utils._
import utils.Reporters._
import utils.Automatons._

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

            val dfa: Automaton[String] = new NFAEffectRepresentation(effect).getNFA.map(_.toSimpleString).determinize.minimize

            for (contr <- fun.contrEffects) {
              val (tpe, reg, res) = contr match {
                case a @ AssertOnlyModified(regex, region) =>
                  val others = dfa - region
                  if (others.isImpossible) {
                    ("Only", regex, "PASSED")
                  } else {
                    ("Only", regex, "FAILED")
                  }
                case a @ AssertUntouched(regex, region) =>
                  val others = dfa intersection region
                  if (others.isImpossible) {
                    ("Not", regex, "PASSED")
                  } else {
                    ("Not", regex, "FAILED")
                  }
              }
              cntAss += 1
              table.addRow(TableRow() | fun.symbol.fullName | tpe | reg.toString | res | sig.toString)
            }
          }
        }

        reporter.dispatch(table.draw _)
        reporter.msg(" -> Checked "+cntAss+" assertions.")
      }
    }
  }
}
