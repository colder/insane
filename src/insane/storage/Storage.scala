package insane
package storage

import org.squeryl._
import PrimitiveTypeMode._
import adapters.H2Adapter
import annotations.Column

trait Storage {
  self: AnalysisComponent =>

  import global._


  def initializeStorage() {
    Class.forName("org.h2.Driver");

    SessionFactory.concreteFactory = Some(()=>
       Session.create(
         java.sql.DriverManager.getConnection("jdbc:h2:~/insane.db", "insane", ""),
         new H2Adapter))

    transaction {
      Database.Hierarchy.create
    }

    transaction {
      for (e <- Database.Hierarchy.entries) {
        println(e.id+": "+e.name+" ("+e.left+","+e.right+")")
      }
    }
  }
}

object Database {

  class HierarchyEntry(val id: Long,
                       val name: String,
                       val parentId: Long,
                       val left: Long,
                       val right: Long) extends KeyedEntity[Long]


  object Hierarchy extends Schema {
    val entries = table[HierarchyEntry]

    on(entries)( b => declare (
      b.id is (indexed, autoIncremented),
      b.name is (unique, dbType("varchar(255)")),
      b.parentId defaultsTo(0l),
      b.parentId is indexed,
      columns(b.left, b.right) are indexed
    ))

    def lookup(name: String): Option[HierarchyEntry] = {
      entries.where(e => e.name === name).headOption
    }

    def subTree(name: String): Set[String] = transaction {
      // 1) we query for the name
      val parents = lookup(name)

      parents match {
        case Some(p) =>
          entries.where(e => e.left between (p.left, p.right)).map(_.name).toSet
        case None =>
          Set()
      }
    }

    def insertChild(childName: String, parentName: Option[String]) = transaction {
      parentName.flatMap(lookup _) match {
        case Some(he) =>
          val r = he.right

          update(entries)(e =>
            where(e.right > r === true)
            set(e.right := e.right.~ + 2)
          )

          update(entries)(e =>
            where(e.left > r === true)
            set(e.left := e.left.~ + 2)
          )

          entries.insert(new HierarchyEntry(0, childName, he.id, r+1, r+2))
        case None =>
          val maxRight = from(entries)(e =>
            select(e.right)
            orderBy(e.right desc)
          ).page(0,1).headOption.getOrElse(0l)

          entries.insert(new HierarchyEntry(0, childName, 0, maxRight+1, maxRight+2))
      }
    }

  }
}

