package experiments.macros

import cats.collections.Diet
import cats.data.NonEmptyList
import experiments.macros.ast
import experiments.macros.ast.Regex
import parsley.templates.{PureParserBridge1, PureParserBridge2}

object bridges {

  object Alt extends PureParserBridge2[Regex[?], Regex[?], Regex[?]] {
    override def apply(left: Regex[?], right: Regex[?]) = ast.Alt(left, right)
  }

  object Capture extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.Capture(inner)
  }

  object Cat extends PureParserBridge1[NonEmptyList[Regex[?]], Regex[?]] {
    override def apply(regexes: NonEmptyList[Regex[?]]): Regex[?] = regexes match {
      case NonEmptyList(head, tail) => tail.foldLeft(head)(ast.Cat(_, _))
    }
  }

  object Lit extends PureParserBridge1[Int, Regex[?]] {
    override def apply(c: Int): Regex[?] = ast.Lit(c)
  }

  object Class extends PureParserBridge1[Diet[Int], Regex[?]] {
    override def apply(cs: Diet[Int]): Regex[?] = ast.Class(cs)
  }

  object NonCapture extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.NonCapture(inner)
  }

  object Opt extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.Opt(inner)
  }

  object Rep0 extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.Rep0(inner)
  }

  object Rep1 extends PureParserBridge1[Regex[?], Regex[?]] {
    override def apply(inner: Regex[?]): Regex[?] = ast.Rep1(inner)
  }
}
