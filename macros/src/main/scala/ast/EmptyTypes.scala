package experiments.macros.ast

import experiments.macros.hchain.HEmpty
import scala.quoted.{Quotes, Type}

trait EmptyTypes { this: Tidy =>
  /* Type of an empty node. In it's own trait for consistency. */
  protected class EmptyType(using Type[Const[HEmpty]]) extends HEmptyType
  protected object EmptyType {
    given Quotes => EmptyType = EmptyType()
  }
}
