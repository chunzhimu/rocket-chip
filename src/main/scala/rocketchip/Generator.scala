// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import cde._
import coreplex._
import java.io.{File, FileWriter}

case class ParsedInputNames(
    targetDir: String,
    topProject: String,
    topModuleClass: String,
    configProject: String,
    configs: String) {
  val configClasses: Seq[String] = configs.split('_')
  val fullConfigClasses: Seq[String] = configClasses.map(configProject + "." + _)
  val fullTopModuleClass: String = topProject + "." + topModuleClass
}

trait HasGeneratorUtilities {
  def getConfig(names: ParsedInputNames): Config = {
    names.fullConfigClasses.foldRight(new Config()) { case (currentName, config) =>
      val currentConfig = try {
        Class.forName(currentName).newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException(s"""Unable to find part "$currentName" from "${names.configs}", did you misspell it?""", e)
      }
      currentConfig ++ config
    }
  }

  def getParameters(names: ParsedInputNames): Parameters = getParameters(getConfig(names))

  def getParameters(config: Config): Parameters = Parameters.root(config.toInstance)

  import chisel3.internal.firrtl.Circuit
  def elaborate(names: ParsedInputNames, params: Parameters): Circuit = {
    val gen = () =>
      Class.forName(names.fullTopModuleClass)
        .getConstructor(classOf[cde.Parameters])
        .newInstance(params)
        .asInstanceOf[Module]

    Driver.elaborate(gen)
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname) 
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
}

trait Generator extends App with HasGeneratorUtilities {
  lazy val names = {
    require(args.size == 5, "Usage: sbt> " + 
      "run TargetDir TopModuleProjectName TopModuleName ConfigProjectName ConfigNameString")
    ParsedInputNames(
      targetDir = args(0),
      topProject = args(1),
      topModuleClass = args(2),
      configProject = args(3),
      configs = args(4))
  }
  lazy val config = getConfig(names)
  lazy val world = config.toInstance
  lazy val params = Parameters.root(world)
  lazy val circuit = elaborate(names, params)
}

object RocketChipGenerator extends Generator {
  val longName = names.topModuleClass + "." + names.configs
  val td = names.targetDir
  Driver.dumpFirrtl(circuit, Some(new File(td, s"$longName.fir"))) // FIRRTL
  TestGeneration.addSuite(new RegressionTestSuite(params(RegressionTestNames)))
  writeOutputFile(td, s"$longName.d", TestGeneration.generateMakefrag) // Coreplex-specific test suites
  writeOutputFile(td, s"$longName.prm", ParameterDump.getDump) // Parameters flagged with Dump()
  writeOutputFile(td, s"${names.configs}.knb", world.getKnobs) // Knobs for DSE
  writeOutputFile(td, s"${names.configs}.cst", world.getConstraints) // Constraints for DSE
  writeOutputFile(td, s"${names.configs}.cfg", params(ConfigString).toString) // String for software
}
