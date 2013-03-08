import sbt._
import Process._
import Keys._

object Insane extends Build {

    def scriptFile = file(".") / "scalac-insane"

    val scriptTask = TaskKey[Unit]("script", "Generate scalac-insane") <<= (streams, scalaHome, dependencyClasspath in Compile, classDirectory in Compile) map { (s, home, deps, out) =>

      if (!scriptFile.exists()) {

        val depsPaths = deps.map(_.data.absolutePath)

        s.log.info("Generating script...")
        try {
          val scalahome = depsPaths.find(_.endsWith("lib/scala-library.jar")) match {
            case None => throw new Exception("Couldn't guess scalaHome.")
            case Some(p) => file(p.substring(0, p.length - 21))
          }

          val nl = System.getProperty("line.separator")
          val f = scriptFile
          val fw = new java.io.FileWriter(f)
          fw.write("#!/bin/bash" + nl)

          fw.write("SCALACLASSPATH=\"")
          fw.write((Seq(out.absolutePath, scalahome / "lib/scala-compiler.jar") ++ depsPaths).mkString(":"))
          fw.write("\"" + nl + nl)

          fw.write("SCALABOOTCLASSPATH=\"")
          fw.write(depsPaths.filter(_ endsWith "scala-library.jar").mkString(":"))
          fw.write("\"" + nl + nl)


          val props = if (System.getProperty("sun.arch.data.model") == "64") {
            "-Xmx4G -Xms1024M"
          } else {
            "-Xmx2G -Xms512M"
          }

          fw.write("java "+props+" -classpath ${SCALACLASSPATH} -Dscala.home=\"$SCALA_HOME\" -Dscala.usejavacp=true \\"+nl)
          fw.write("    scala.tools.nsc.MainGenericRunner -classpath ${SCALACLASSPATH} \\"+nl)
          fw.write("        insane.Main -classpath /dev/null $@")
          f.setExecutable(true)
        } catch {
          case e => s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
        }
      }
    }

    val quitTask = TaskKey[Unit]("quit", "Alias of exit") := {
      sys.exit(0)
    }

    val nameKey         = SettingKey[String]("name", "Name of the project")
    val versionKey      = SettingKey[String]("version", "Version")

    object InsaneLibProject {

      val settings = Seq(nameKey := "Insane-Lib",
                         versionKey := "1.0.0")
    }

    //lazy val rootLib  = Project(id = "insane-lib",
    //                        base = file("."),
    //                        settings = Project.defaultSettings ++ Seq(quitTask) ++ InsaneLibProject.settings)

    object InsaneProject {
      val settings = Seq(scriptTask,
                         quitTask,
                         nameKey := "Insane",
                         versionKey := "1.0.0")
    }

    lazy val root = Project(id = "insane",
                            base = file("."),
                            settings = Project.defaultSettings ++ Seq(quitTask, scriptTask) ++ InsaneProject.settings)

}
