package insane
package storage

import org.squeryl._
import org.squeryl.internals.StatementWriter
import PrimitiveTypeMode._
import adapters.H2Adapter
import adapters.MySQLAdapter
import java.sql.DriverManager

import annotations.Column
import utils.Hash

trait Storage {
  self: AnalysisComponent =>

  import global._


  def initializeStorage() {

    settings.databaseType match {
      case "mysql" =>
        Class.forName("com.mysql.jdbc.Driver");

        SessionFactory.concreteFactory = Some(()=>
            Session.create(
             DriverManager.getConnection(settings.databaseDSN, settings.databaseUsername,  settings.databasePassword),
             new MySQLAdapter {
              override def writeInsert[T](o: T, t: Table[T], sw: StatementWriter):Unit = {

                val o_ = o.asInstanceOf[AnyRef]    
                val f = t.posoMetaData.fieldsMetaData.filter(fmd => !fmd.isAutoIncremented)

                sw.write("replace into ");
                sw.write(quoteName(t.prefixedName));
                sw.write(" (");
                sw.write(f.map(fmd => quoteName(fmd.columnName)).mkString(", "));
                sw.write(") values ");
                sw.write(
                  f.map(fmd => writeValue(o_, fmd, sw)
                ).mkString("(",",",")"));
              }
             
             }))

        Database.active = true

      case "h2" =>
        Class.forName("org.h2.Driver");

        SessionFactory.concreteFactory = Some(()=>
            Session.create(
             DriverManager.getConnection(settings.databaseDSN, settings.databaseUsername,  settings.databasePassword),
             new H2Adapter))

        Database.active = true

      case _ =>
    }

    if (settings.createTables) {
      val connection = DriverManager.getConnection(settings.databaseDSN, settings.databaseUsername,  settings.databasePassword)

      try {
        val sql = """
          CREATE TABLE IF NOT EXISTS `HierarchyEntry` (
            `id` bigint(20) NOT NULL AUTO_INCREMENT,
            `name` varchar(255) NOT NULL,
            `lft` bigint(20) NOT NULL,
            `rht` bigint(20) NOT NULL,
            PRIMARY KEY (`id`),
            UNIQUE KEY `name` (`name`),
            KEY `lft` (`lft`),
            KEY `rht` (`rht`),
            KEY `lftrht` (`lft`,`rht`)
          ) ENGINE=InnoDB  DEFAULT CHARSET=utf8"""

        connection.prepareStatement(sql).execute
      } catch {
        case e => reporter.warn("Failed to create table HierarchyEntry: "+e.getMessage)
      }

      try {
        

        val sql = """
          CREATE TABLE IF NOT EXISTS `EnvEntry` (
            `id` varchar(255) NOT NULL,
            `name` text NOT NULL,
            `isSynthetic` tinyint(1) NOT NULL,
            `env` text NOT NULL,
            PRIMARY KEY (`id`(255))
          ) ENGINE=InnoDB DEFAULT CHARSET=utf8"""

        connection.prepareStatement(sql).execute
      } catch {
        case e => reporter.warn("Failed to create table EnvEntry: "+e.getMessage)
      }
    }
  }
}

object Database {
  var active: Boolean = false

  class EnvEntry(val id: String,
                 val name: String,
                 val env: String,
                 val isSynthetic: Boolean)

  object Env extends Schema {
    val entries = table[EnvEntry]

    on(entries)( b => declare (
      b.name is (unique, dbType("TEXT")),
      b.env  is (dbType("TEXT")),
      b.isSynthetic is indexed
    ))

    def createTables() = transaction {
      create
    }

    def transLookup(name: String): Option[EnvEntry] = transaction {
      entries.where(e => e.name === name).headOption
    }
    def lookup(name: String): Option[EnvEntry] = {
      entries.where(e => e.name === name).headOption
    }

    def idFromName(str: String) = {
      Hash.sha1(str)
    }

    def lookupEnv(name: String): Option[String] = transaction {
      from(entries)( e =>
        where(e.name === name and e.id === idFromName(name))
        select(e.env)
        orderBy(e.isSynthetic desc)
      ).headOption
    }

    def lookupPriorityEnv(name: String): Option[String] = transaction {
      from(entries)( e =>
        where(e.name === name and e.id === idFromName(name) and e.isSynthetic === true)
        select(e.env)
      ).headOption
    }


    def insertAll(es: Traversable[(String, String, Boolean)]) = transaction {
      entries.insert(es.map{ case (name, env, synth) => new EnvEntry(idFromName(name), name, env, synth)}.toList)
    }
  }

  class HierarchyEntry(val id: Long,
                       val name: String,
                       val lft: Long,
                       val rht: Long) extends KeyedEntity[Long]


  object Hierarchy extends Schema {
    val entries = table[HierarchyEntry]

    on(entries)( b => declare (
      b.id is (indexed, autoIncremented),
      b.name is (unique, dbType("varchar(255)")),
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
      entries.insert(es.map{ case (name, lft, rht) => new HierarchyEntry(0, name, lft, rht)}.toList)
    }

    def insertDirectChild(childName: String, lft: Long, rht: Long) = transaction {
        entries.insert(new HierarchyEntry(0, childName, lft, rht))
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

          entries.insert(new HierarchyEntry(0, childName, r+1, r+2))
        case None =>
          val maxRight = from(entries)(e =>
            select(e.rht)
            orderBy(e.rht desc)
          ).page(0,1).headOption.getOrElse(0l)

          entries.insert(new HierarchyEntry(0, childName, maxRight+1, maxRight+2))
      }
    }

  }
}

