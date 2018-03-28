package Rule

import Configs.DatabaseConfig
import Result.{Failed, Passed}

/**
 * Composition Service component to be used as a mixin.
 * Provides a CompositionService
 *
 * @author Kai Sassnowski
 */
trait CompositionServiceComponent {
  this: RuleServiceComponent =>

  val compositionService: CompositionService

  /**
   * Class responsible for evaluating compositions.
   *
   * @author Kai Sassnowski
   */
  class CompositionService {

    /**
     * Evaluates a composition.
     *
     * @param c The composition to evaluate.
     * @return The result of the evaluation.
     */
    def evaluate(c: Composition, dbConfig: DatabaseConfig): Boolean = c match {
      case Value(r) => ruleService.evaluate(r, dbConfig) match {
        case Passed(_) => true
        case Failed(_) => false
      }
      case And(l, r) => evaluate(l, dbConfig) && evaluate(r, dbConfig)
      case Or(l, r) => evaluate(l, dbConfig) || evaluate(r, dbConfig)
      case XOr(l, r) => evaluate(l, dbConfig) ^ evaluate(r, dbConfig)
      case NAnd(l, r) => !(evaluate(l, dbConfig) && evaluate(r, dbConfig))
    }
  }
}

