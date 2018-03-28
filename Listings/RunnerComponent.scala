[...]
/**
 * Runner component to be used as a mixin. Provides a test runner.
 *
 * @author Kai Sassnowski
 */
trait RunnerComponent {
  this: RuleParserComponent with
    RuleServiceComponent with
    ReporterComponent =>

  val runner: DefaultRunner

  /**
   * Class in charge of executing a complete validation.
   * @author Kai Sassnowski
   */
  class DefaultRunner {
    /**
     * Main entry point. Reads all rules provided by the source file and evaluates them.
     *
     * @param c Config containing all command line arguments.
     */
    def run(c: CliConfig): Unit = {
      implicit val cliConfig = setRulePath(c)
      val masterConfig = ConfigFactory.parseString(Source.fromFile(cliConfig.source).mkString)
      val databaseConfig = DatabaseConfig(
        masterConfig.getString("database.url"),
        masterConfig.getString("database.driver"),
        masterConfig.getString("database.user"),
        masterConfig.getString("database.password")
      )
      val rules = createRules(masterConfig)
      val report = reporter.report(executeRules(rules, databaseConfig))

      if (cliConfig.toConsole) {
        println(report)
      } else {
        reporter.save(cliConfig.out, report)
      }
    }

    /**
     * Creates a list of rules that need to be executed.
     *
     * @param c Config containing the parsed master rule file.
     * @param cliConfig Config containing all command line parameters.
     * @return List of Rules to be executed.
     */
    def createRules(c: Config)(implicit cliConfig: CliConfig): List[Rule] =
      c.getStringList("rules") map { r => ruleParser.fromFile(r + ".conf") } toList

    /**
     * Evaluate a list of rules and returns the resulting ResultSet.
     *
     * @param rs The rules to evaluate.
     * @param dbConfig The database config to use.
     * @return The ResultSet containing the results of the evaluations.
     */
    def executeRules(rs: List[Rule], dbConfig: DatabaseConfig): ResultSet =
      rs map { r => ruleService.evaluate(r, dbConfig) }

    /**
     * Sets the rule path for the config. If an explicit path was provided through
     * the CLI then simply returns the original config. Otherwise it uses the base path
     * of the provided master file.
     *
     * @param c The CliConfig
     * @return Either the original config if nothing needed to be changed or the new config with the rulePath set.
     */
    private def setRulePath(c: CliConfig): CliConfig = {
      if (c.rulePath == "") {
        val masterPath = c.source.getAbsolutePath.split("/").init.mkString("/")
        c.copy(rulePath = masterPath)
      } else c
    }
  }
}
