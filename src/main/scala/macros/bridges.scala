package experiments.macros

import cats.collections.Diet
import cats.data.NonEmptyList
import experiments.macros.ast
import experiments.macros.ast.Regex
import parsley.templates.{PureParserBridge1, PureParserBridge2}
import parsley.bridges.ParserSingletonBridge
import parsley.Parsley
import parsley.Parsley.pure
import scala.quoted.Quotes

object bridges {

  type QuotesToRegex = Quotes ?=> Regex[?]

  object Alt extends PureParserBridge2[QuotesToRegex, QuotesToRegex, QuotesToRegex] {
    override def apply(left: QuotesToRegex, right: QuotesToRegex): QuotesToRegex = ast.Alt(left, right)
  }

  object Capture extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = ast.Capture(inner)
  }

  object Cat extends PureParserBridge1[NonEmptyList[QuotesToRegex], QuotesToRegex] {
    override def apply(regexes: NonEmptyList[QuotesToRegex]): QuotesToRegex = {
      val NonEmptyList(head, tail) = regexes
      tail.foldLeft(head)(ast.Cat(_, _))
    }
  }

  object Dot extends ParserSingletonBridge[QuotesToRegex] {
    override protected def singleton: Parsley[QuotesToRegex] = pure(ast.Dot())
  }

  object Lit extends PureParserBridge1[Int, QuotesToRegex] {
    override def apply(c: Int): QuotesToRegex = ast.Lit(c)
  }

  object Class extends PureParserBridge1[Diet[Int], QuotesToRegex] {
    override def apply(cs: Diet[Int]): QuotesToRegex = ast.Class(cs)
  }

  object NonCapture extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = ast.NonCapture(inner)
  }

  object Opt extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = ast.Opt(inner)
  }

  object Rep0 extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = ast.Rep0(inner)
  }

  object Rep1 extends PureParserBridge1[QuotesToRegex, QuotesToRegex] {
    override def apply(inner: QuotesToRegex): QuotesToRegex = ast.Rep1(inner)
  }
}
