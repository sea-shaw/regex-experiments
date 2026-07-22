package experiments.macros

import cats.collections.Diet
import cats.data.NonEmptyList
import experiments.macros.ast
import experiments.macros.ast.Regex
import parsley.templates.{PureParserBridge1, PureParserBridge2, PureParserBridge3}
import scala.quoted.Quotes
import experiments.macros.hcollections.hchain.HChain

object bridges {

  type QuotesToRegex = Quotes => Regex[?]

  object Alt extends PureParserBridge2[QuotesToRegex, QuotesToRegex, QuotesToRegex] {
    override def apply(left: QuotesToRegex, right: QuotesToRegex) = { q =>
      ast.Alt(left(q), right(q))(using q)
    }
  }

  object Capture extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = { q =>
      ast.Capture(inner(q))(using q)
    }
  }

  object Cat extends PureParserBridge1[NonEmptyList[QuotesToRegex], QuotesToRegex] {
    override def apply(regexes: NonEmptyList[QuotesToRegex]): QuotesToRegex = { q =>
      regexes match {
        case NonEmptyList(head, tail) => tail.foldLeft(head(q)){ (left, right) =>
          ast.Cat(left, right(q))(using q)
        }
      }
    }
  }

  object Dot extends PureParserBridge1[Quotes, Regex[?]] {
    override def apply(q: Quotes): Regex[?] = ast.Dot(using q)
  }

  object Lit extends PureParserBridge1[Int, QuotesToRegex] {
    override def apply(c: Int): QuotesToRegex = { q =>
      ast.Lit(c)(using q)
    }
  }

  object Class extends PureParserBridge1[Diet[Int], QuotesToRegex] {
    override def apply(cs: Diet[Int]): QuotesToRegex = { q =>
      ast.Class(cs)(using q)
    }
  }

  object NonCapture extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = { q =>
      ast.NonCapture(inner(q))
    }
  }

  object Opt extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = { q =>
       ast.Opt(inner(q))(using q)
    }
  }

  object Rep0 extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = { q => 
      ast.Rep0(inner(q))(using q)
    }
  }

  object Rep1 extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = { q =>
      ast.Rep1(inner(q))(using q)
    }
  }
}
