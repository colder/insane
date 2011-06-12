package insane
package storage

import org.squeryl._
import PrimitiveTypeMode._
import adapters.H2Adapter
import adapters.MySQLAdapter

import annotations.Column

trait Storage {
  self: AnalysisComponent =>

  import global._


  def initializeStorage() {

    settings.databaseType match {
      case "mysql" =>
        Class.forName("com.mysql.jdbc.Driver");
        SessionFactory.concreteFactory = Some(()=>
            Session.create(
             java.sql.DriverManager.getConnection(settings.databaseDSN, settings.databaseUsername,  settings.databasePassword),
             new MySQLAdapter))
      case "h2" =>
        Class.forName("org.h2.Driver");
        SessionFactory.concreteFactory = Some(()=>
            Session.create(
             java.sql.DriverManager.getConnection(settings.databaseDSN, settings.databaseUsername,  settings.databasePassword),
             new H2Adapter))
      case _ =>
    }

  }

  // Helpers to store and retreive dynamic data such as symbols
  case class DeflatedSymbol(fullName: String, isModuleClass: Boolean, isError: Boolean) {
    override def toString = {
      if (isError) {
        fullName+":e"
      } else if(isModuleClass) {
        fullName+":m"
      } else {
        fullName+":c"
      }
    }

    def inflate: Symbol = {
      if (isError) {
        NoSymbol
      } else if (isModuleClass) {
        definitions.getModule(fullName) 
      } else {
        definitions.getClass(fullName) 
      }
    }
  }

  object DeflatedSymbol {
    def fromString(str: String): DeflatedSymbol = str.split(":", 2).toList match {
      case fullName :: "m" :: Nil =>
        DeflatedSymbol(fullName, true, false)
      case fullName :: "c" :: Nil =>
        DeflatedSymbol(fullName, false, false)
      case fullName :: "e" :: Nil =>
        DeflatedSymbol(fullName, false, true)
      case _ =>
        DeflatedSymbol("?", false, true)
    }
  }

  class InflatedSymbol(sym: Symbol) {
    def deflate = {
      assert(sym.isClass, "Trying to deflate a non-class symbol: "+sym)

      val (fullName, error) = try { 
        (sym.fullName, false)
      } catch { 
        case _ => 
          (sym.name.toString, true)
      }

      DeflatedSymbol(fullName, sym.isModuleClass, error)
    }

    def deflatedString = deflate.toString
  }

  implicit def symToInflSym(s: Symbol): InflatedSymbol = new InflatedSymbol(s)
}

object Database {

  class HierarchyEntry(val id: Long,
                       val name: String,
                       val parentId: Long,
                       val lft: Long,
                       val rht: Long) extends KeyedEntity[Long]


  object Hierarchy extends Schema {
    val entries = table[HierarchyEntry]

    on(entries)( b => declare (
      b.id is (indexed, autoIncremented),
      b.name is (unique, dbType("varchar(255)")),
      b.parentId defaultsTo(0l),
      b.parentId is indexed,
      b.lft is indexed,
      b.rht is indexed,
      columns(b.lft, b.rht) are indexed
    ))

    def createTables() = transaction {
      create
    }

    def transLookup(name: String): Option[HierarchyEntry] = transaction {
      entries.where(e => e.name === name).headOption
    }
    def lookup(name: String): Option[HierarchyEntry] = {
      entries.where(e => e.name === name).headOption
    }

    def subTree(name: String): Set[String] = transaction {
      // 1) we query for the name
      val parents = lookup(name)

      parents match {
        case Some(p) =>
          entries.where(e => e.lft between (p.lft, p.rht)).map(_.name).toSet
        case None =>
          Set()
      }
    }

    def insertAll(es: Set[(String, Long, Long)]) = transaction {
      entries.insert(es.map{ case (name, lft, rht) => new HierarchyEntry(0, name, 0, lft, rht)}.toList)
    }

    def insertDirectChild(childName: String, lft: Long, rht: Long) = transaction {
        entries.insert(new HierarchyEntry(0, childName, 0, lft, rht))
    }

    def insertChild(childName: String, parentName: Option[String]) = transaction {
      parentName.flatMap(lookup _) match {
        case Some(he) =>
          val r = he.rht

          update(entries)(e =>
            where(e.rht > r === true)
            set(e.rht := e.rht.~ + 2)
          )

          update(entries)(e =>
            where(e.lft > r === true)
            set(e.lft := e.lft.~ + 2)
          )

          entries.insert(new HierarchyEntry(0, childName, he.id, r+1, r+2))
        case None =>
          val maxRight = from(entries)(e =>
            select(e.rht)
            orderBy(e.rht desc)
          ).page(0,1).headOption.getOrElse(0l)

          entries.insert(new HierarchyEntry(0, childName, 0, maxRight+1, maxRight+2))
      }
    }

  }
}

