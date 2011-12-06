name := "Insane"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.4"

libraryDependencies += "com.h2database" % "h2" % "1.2.127"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.15"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.1"

(scalaSource in Compile) <<= sourceDirectory apply { bd =>   bd / "insane" }
