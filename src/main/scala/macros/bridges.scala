package experiments.macros

import cats.collections.Diet
import cats.data.NonEmptyList
import experiments.macros.ast
import experiments.macros.ast.Regex
import parsley.templates.{PureParserBridge1, PureParserBridge2, PureParserBridge3}
import scala.quoted.Quotes
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.ast.Rep

object bridges {

  object Alt extends PureParserBridge3[Regex[?], Regex[?], Quotes, Regex[?]] {
    override def apply(left: Regex[?], right: Regex[?], q: Quotes) = ast.Alt(left, right)(using q)
  }

  object Capture extends PureParserBridge2[Regex[?], Quotes, Regex[?]] {
    override def apply(inner: Regex[?], q: Quotes): Regex[?] = ast.Capture(inner)(using q)
  }

  object Cat extends PureParserBridge2[NonEmptyList[Regex[?]], Quotes, Regex[?]] {
    override def apply(regexes: NonEmptyList[Regex[?]], q: Quotes): Regex[?] = regexes match {
      case NonEmptyList(head, tail) => tail.foldLeft(head)(ast.Cat(_, _)(using q))
    }
  }

  object Dot extends PureParserBridge1[Quotes, Regex[?]] {
    override def apply(q: Quotes): Regex[?] = ast.Dot(using q)
  }

  object Lit extends PureParserBridge2[Int, Quotes, Regex[?]] {
    override def apply(c: Int, q: Quotes): Regex[?] = ast.Lit(c)(using q)
  }

  object Class extends PureParserBridge2[Diet[Int], Quotes, Regex[?]] {
    override def apply(cs: Diet[Int], q: Quotes): Regex[?] = ast.Class(cs)(using q)
  }

  object NonCapture extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.NonCapture(inner)
  }

  object Opt extends PureParserBridge2[Regex[?], Quotes, Regex[?]] {
    override def apply(inner: Regex[?], q: Quotes): Regex[?] = ast.Opt(inner)(using q)
  }

  object Rep0 extends PureParserBridge2[Regex[?], Quotes, Regex[?]] {
    override def apply(inner: Regex[?], q: Quotes): Regex[?] = ast.Rep0(inner)(using q)
  }

  object Rep1 extends PureParserBridge2[Regex[?], Quotes, Regex[?]] {
    override def apply(inner: Regex[?], q: Quotes): Regex[?] = ast.Rep1(inner)(using q)
  }
}
