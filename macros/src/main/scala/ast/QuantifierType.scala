package experiments.macros.ast

sealed trait QuantifierType
case object Greedy extends QuantifierType
case object Reluctant extends QuantifierType
case object Possessive extends QuantifierType
