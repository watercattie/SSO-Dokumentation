package Core

import java.io.File

import Configs.CliConfig

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[CliConfig]("dbvalidate") {
      head("dbvalidate", "0.1.0")
      opt[String]('o', "out") optional() action { (x, c) =>
        c.copy(out = x) } text "The output file containing the results. Defaults to 'result.xml' in the current directory."
      arg[File]("<mainfile>") required() action { (x, c) =>
        c.copy(source = x) } text "The master file"
      arg[String]("<rulepath>") optional() action { (x, c) =>
        c.copy(rulePath = x) } text "The path for the rule files. Defaults to the base path of the mainfile."
      opt[Unit]("to-console") optional() action { (_, c) =>
        c.copy(toConsole = true) } text "If this flag is set the results will not be saved to a file but instead printed to the console"
      help("help") text "prints this usage text"
    }

    parser.parse(args, CliConfig()) match {
      case Some(c) => ComponentRegistry.runner.run(c)
      case None =>
    }
  }

}
