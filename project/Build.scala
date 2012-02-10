import sbt._
import Process._
import Keys._

object Insane extends Build {

    def scriptFile = file(".") / "scalac-insane"

    val scriptTask = TaskKey[Unit]("script", "Generate scalac-insane") <<= (streams, dependencyClasspath in Compile, classDirectory in Compile) map { (s, deps, out) =>
      if (!scriptFile.exists()) {
        s.log.info("Generating script...")
        try {
          val nl = System.getProperty("line.separator")
          val f = scriptFile
          val fw = new java.io.FileWriter(f)
          fw.write("#!/bin/bash" + nl)

          val depsPaths = deps.map(_.data.absolutePath)
          fw.write("SCALACLASSPATH=\"")
          fw.write((Seq(out.absolutePath) ++ depsPaths).mkString(":"))
          fw.write("\"" + nl + nl)

          fw.write("SCALABOOTCLASSPATH=\"")
          fw.write(depsPaths.filter(_ endsWith "scala-library.jar").mkString(":"))
          fw.write("\"" + nl + nl)


          val props = if (System.getProperty("sun.arch.data.model") == "64") {
            "-Xmx4G -Xms1024M"
          } else {
            "-Xmx2G -Xms512M"
          }

          fw.write("JAVA_OPTS=\""+props+"\" scala -nobootcp -classpath ${SCALACLASSPATH} \\" + nl)
          fw.write("  insane.Main -bootclasspath ${SCALABOOTCLASSPATH} -classpath /dev/null $@" + nl)
          fw.close
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
    val scalaVersionKey = SettingKey[String]("scalaVersion", "Scala Version")
    val versionKey      = SettingKey[String]("version", "Version")

    object InsaneLibProject {

      val settings = Seq(nameKey := "Insane-Lib",
                         scalaVersionKey := "2.9.1",
                         versionKey := "1.0.0")
    }

    //lazy val rootLib  = Project(id = "insane-lib",
    //                        base = file("."),
    //                        settings = Project.defaultSettings ++ Seq(quitTask) ++ InsaneLibProject.settings)

    object InsaneProject {
      val settings = Seq(scriptTask,
                         quitTask,
                         nameKey := "Insane",
                         scalaVersionKey := "2.9.1",
                         versionKey := "1.0.0")
    }

    lazy val root = Project(id = "insane",
                            base = file("."),
                            settings = Project.defaultSettings ++ Seq(quitTask, scriptTask) ++ InsaneProject.settings)

}
