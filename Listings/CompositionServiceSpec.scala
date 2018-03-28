package unit

import Configs.DatabaseConfig
import Core.TestEnvironment
import Result.{Passed, Failed}
import Rule._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class CompositionServiceSpec extends FlatSpec with Matchers with TestEnvironment with MockFactory {

  override val ruleService = stub[RuleService]

  val dbConfig = DatabaseConfig(
    url="jdbc:sqlite:./src/db.db",
    driver="org.SQLite.Driver"
  )

  val rule1 = SimpleRule("Rule 1", "Description", "SELECT 1 > 2")
  val rule2 = SimpleRule("Rule 2", "Description", "SELECT 2 > 1")

  "An AND composition" should "return true if both rules evaluate to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Passed(rule2))

    compositionService.evaluate(And(Value(rule2), Value(rule2)), dbConfig) should be(true)
  }

  it should "return false if only one rule evaluates to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Passed(rule2))

    compositionService.evaluate(And(Value(rule1), Value(rule2)), dbConfig) should be(false)
  }

  it should "return false if both rules evaluate to false" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Passed(rule2))

    compositionService.evaluate(And(Value(rule1), Value(rule1)), dbConfig) should be(false)
  }

  "An OR composition" should "return true if both rules evaluate to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Passed(rule2))

    compositionService.evaluate(Or(Value(rule2), Value(rule2)), dbConfig) should be(true)
  }

  it should "return true if only one rule evaluates to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Passed(rule2))

    compositionService.evaluate(Or(Value(rule1), Value(rule2)), dbConfig) should be(true)
    compositionService.evaluate(Or(Value(rule2), Value(rule1)), dbConfig) should be(true)
  }

  it should "return false if both rules evaluate to false" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))

    compositionService.evaluate(Or(Value(rule1), Value(rule1)), dbConfig) should be(false)
  }

  "An XOR composition" should "return false if both rules evaluate to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))

    compositionService.evaluate(XOr(Value(rule1), Value(rule1)), dbConfig) should be(false)
  }

  it should "return true if only of the rules evaluates to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Failed(rule2))

    compositionService.evaluate(XOr(Value(rule1), Value(rule2)), dbConfig) should be(true)
    compositionService.evaluate(XOr(Value(rule2), Value(rule1)), dbConfig) should be(true)
  }

  it should "return false if both rules evaluate to false" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))

    compositionService.evaluate(XOr(Value(rule1), Value(rule1)), dbConfig) should be(false)
  }

  "A NAND composition" should "return false if both rules evaluate to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))

    compositionService.evaluate(NAnd(Value(rule1), Value(rule1)), dbConfig) should be(false)
  }

  it should "return true if only one of the rules evaluates to true" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Failed(rule2))

    compositionService.evaluate(NAnd(Value(rule1), Value(rule2)), dbConfig) should be(true)
    compositionService.evaluate(NAnd(Value(rule2), Value(rule1)), dbConfig) should be(true)
  }

  it should "return true if both rules evaluate to false" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Failed(rule1))

    compositionService.evaluate(NAnd(Value(rule1), Value(rule1)), dbConfig) should be(true)
  }

  "#evaluate()" should "correctly evaluate 'false OR (true XOR false)" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Failed(rule2))

    val composition = Or(Value(rule2), XOr(Value(rule1), Value(rule2)))
    compositionService.evaluate(composition, dbConfig) should be(true)
  }

  it should "correctly evaluate 'true NAND (false OR (true XOR true))'" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Failed(rule2))

    val composition = NAnd(Value(rule1), Or(Value(rule2), XOr(Value(rule1), Value(rule1))))
    compositionService.evaluate(composition, dbConfig) should be(true)
  }

  it should "correctly evaluate 'true XOR (true NAND (false OR (true NAND true)))" in {
    (ruleService.evaluate _).when(rule1, dbConfig).returns(Passed(rule1))
    (ruleService.evaluate _).when(rule2, dbConfig).returns(Failed(rule2))

    val composition = XOr(Value(rule1), NAnd(Value(rule1), Or(Value(rule2), NAnd(Value(rule1), Value(rule1)))))
    compositionService.evaluate(composition, dbConfig) should be(false)
  }
}

