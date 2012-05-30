package insane
package utils

import Reporters._

case class Table(columns: Seq[TableColumn]) {
  var rows = Seq[TableRow]()

  def addRow(row: TableRow) = {
    if (row.colSize == columns.size) {
      rows = rows :+ row
    } else {
      sys.error("Invalid row: "+row)
    }
  }

  def draw(reporterHandler: ReporterHandler) {
    // compute max sizes of all columns

    reporterHandler match {
      case ht: HTMLReporterHandler =>
        ht.printText("<table>")
        ht.printText("<tr>")
        for (c <- columns) {
          ht.printText("<th>"+ht.escape(c.title)+"</th>")
        }
        ht.printText("</tr>")

        for (r <- rows) {
          ht.printText("<tr>")
          for (d <- r.data) {
            ht.printText("<td>"+ht.escape(d)+"</td>")
          }
          ht.printText("</tr>")
        }

        ht.printText("</table>")

      case _ =>
        def printer(str: String) = reporterHandler.printText(str + "\n")

        var colSizes = collection.mutable.Map[Int, Int]() ++ columns.zipWithIndex.map { case (c, i) => (i -> c.title.size) }

        for (r <- rows) {
          for ((data, i) <- r.data.zipWithIndex) {
            colSizes(i) = colSizes(i).max(columns(i).sizeOf(data)).max(4)
          }
        }

        def filler(s: String)(i: Int) = {
          s*(colSizes(i)+2)
        }

        def padded(data: Int => String)(i: Int) = {
          " "+columns(i).getString(data(i), colSizes(i))+" "
        }

        def getRow(l: String, m: String, r: String)(data: Int => String) = {
          var line = ""
          for ((c, i) <- columns.zipWithIndex) {
            if (i == 0) {
              line += l+data(i)
            } else if (i == columns.size-1) {
              line += m+data(i)+r
            } else{
              line += m+data(i)
            }
          }
          line
        }

        printer(getRow("┌", "┬", "┐")(filler("─")))
        printer(getRow("│", "│", "│")(padded(columns(_).title)))
        printer(getRow("├", "┼", "┤")(filler("─")))

        for(r <- rows) {
          printer(getRow("│", "│", "│")(padded(r.data(_))))
        }

        printer(getRow("└", "┴", "┘")(filler("─")))

    }
  }
}

case class TableColumn(title: String, maxLength: Option[Int]) {
  def sizeOf(data: String) = maxLength match {
    case Some(i) => i.min(data.size)
    case None    => data.size
  }

  def getString(data: String, maxSize: Int) = {
    if (data.size > maxSize && maxSize > 4) {
      data.substring(0, maxSize-3)+"..."
    } else {
      data+" "*(maxSize-data.size)
    }
  }
}

case class TableRow(data: Seq[String] = Seq()) {
  val colSize = data.size

  def |(s: String) = TableRow(data :+ s)
}
