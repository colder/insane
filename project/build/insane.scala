import sbt._

class InsaneProject(info: ProjectInfo) extends DefaultProject(info) with FileTasks {

  override def outputDirectoryName = "bin"
  override def dependencyPath      = "lib"
  override def shouldCheckOutputDirectories = false

  lazy val plugin         = project(".", "insane: Interprocedural Shape Analysis Engine", new PluginProject(_))

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
      fw.write(plugin.jarPath.absolutePath + ":")
      fw.write("\"" + nl + nl)
      fw.write("SCALACCLASSPATH=\"")
      fw.write(plugin.jarPath.absolutePath)
      fw.write("\"" + nl + nl)
      fw.write("LD_LIBRARY_PATH=" + ("." / "lib-bin").absolutePath + " \\" + nl)
      fw.write("java -Xmx1024M \\" + nl)

      // This is a hack :(
      val libStr = (buildLibraryJar.absolutePath).toString
      fw.write("    -Dscala.home=" + libStr.substring(0, libStr.length-21) + " \\" + nl)

      fw.write("    -classpath ${SCALAINSANECLASSPATH} \\" + nl)
      fw.write("  scala.tools.nsc.Main -Xplugin:" + plugin.jarPath.absolutePath + " -classpath ${SCALACCLASSPATH} $@" + nl)
      fw.close
      f.setExecutable(true)
      None
    } catch {
      case e => Some("There was an error while generating the script file: " + e.getLocalizedMessage)
    }
  }) dependsOn(plugin.`package`) describedAs("Produce the runner script.")

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
  class PluginProject(info: ProjectInfo) extends PersonalizedProject(info) {
    override def outputPath = "bin" / "insane"
    override def mainScalaSourcePath = "src" / "insane"
    override def mainResourcesPath   = "resources" / "insane"
  }
}
