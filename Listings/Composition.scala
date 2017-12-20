package Rule

/**
 * An ADT representing a composition of rules.
 *
 * Each branch of the composition is itself a composition. A composition
 * containing only a single rule represents a leaf of the tree and is called
 * a Value.
 */
sealed trait Composition

case class And(left: Composition, right: Composition) extends Composition
case class Or(left: Composition, right: Composition) extends Composition
case class NAnd(left: Composition, right: Composition) extends Composition
case class XOr(left: Composition, right: Composition) extends Composition
case class Value(rule: Rule) extends Composition
