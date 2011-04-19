import sbt._

class InsaneProject(info: ProjectInfo) extends DefaultProject(info) with FileTasks {
  val scalaJline = "org.scala-lang" % "jline" % "2.9.0.RC1"

  val jLinePath = ("." / "lib_managed" / "scala_2.9.0.RC1" / "compile" / "jline-2.9.0.RC1.jar")

  override def outputDirectoryName = "bin"
  override def dependencyPath      = "lib"
  override def shouldCheckOutputDirectories = false
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath   = "resources"

  val scriptPath: Path = "." / "scalac-insane"

  lazy val all = task { None } dependsOn(generateScript) describedAs("Compile everything and produce a script file.")

  override def cleanAction = super.cleanAction dependsOn(cleanScript)

  lazy val generateScript = genScript
  def genScript = fileTask(scriptPath ::Nil)({
    log.info("Generating runner script")
    try {
      val nl = System.getProperty("line.separator")
      val f = scriptPath.asFile
      val fw = new java.io.FileWriter(f)
      fw.write("#!/bin/bash" + nl)
      fw.write("SCALAINSANECLASSPATH=\"")
      fw.write(buildLibraryJar.absolutePath + ":")
      fw.write(buildCompilerJar.absolutePath + ":")
      fw.write(jarPath.absolutePath)
      fw.write("\"" + nl + nl)
      fw.write("JAVA_OPTS=\"-Xmx2G -Xms512M\" scala -classpath ${SCALAINSANECLASSPATH} \\" + nl)
      fw.write("  insane.Main $@" + nl)
      fw.close
      f.setExecutable(true)
      None
    } catch {
      case e => Some("There was an error while generating the script file: " + e.getLocalizedMessage)
    }
  }) dependsOn(`package`) describedAs("Produce the runner script.")

  lazy val cleanScript = clnScript
  def clnScript = task {
    log.info("Deleting runner script")
    scriptPath.asFile.delete
    None
  }

  sealed abstract class PersonalizedProject(info: ProjectInfo) extends DefaultProject(info) {
    override def dependencyPath = "lib"
    override def outputDirectoryName = "bin" 
    override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  }
}
