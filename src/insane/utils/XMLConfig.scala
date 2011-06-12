package insane
package utils

import scala.xml._

class XMLConfig(path: String) {
  private val data = XML.loadFile(path)

  def load(settings: Settings) = {
    val db = data \ "db" head

    settings.databaseType     = (db \ "@type").text
    settings.databaseUsername = (db \ "@username").text
    settings.databasePassword = (db \ "@password").text
    settings.databaseDSN      = (db \ "@dsn").text
  }
}
