package experiments.macros.ast

import experiments.macros.hchain.HEmpty
import scala.quoted.{Quotes, Type}

trait EmptyTypes { this: Tidy =>
  protected class EmptyType(using Type[Const[HEmpty]]) extends HEmptyType
  protected object EmptyType {
    given Quotes => EmptyType = EmptyType()
  }
}
