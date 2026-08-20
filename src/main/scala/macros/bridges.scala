package experiments.macros

import cats.collections.Diet
import cats.data.NonEmptyList
import experiments.macros.ast.AST
import parsley.Parsley
import parsley.Parsley.pure
import parsley.bridges.ParserSingletonBridge
import parsley.templates.{PureParserBridge1, PureParserBridge2}
import scala.quoted.Quotes

object bridges {

  type ToRegex = (ast: AST, q: Quotes) ?=> ast.Regex[?]

  object Dot extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.Dot())
  }

  object Lit extends PureParserBridge1[Int, ToRegex] {
    override def apply(c: Int): ToRegex = ast.Lit(c)
  }

  object Class extends PureParserBridge1[Diet[Int], ToRegex] {
    override def apply(cs: Diet[Int]): ToRegex = ast.Class(cs)
  }

  object LineStart extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.LineStart())
  }

  object LineEnd extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.LineEnd())
  }

  object Capture extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Capture(inner)
  }

  object NonCapture extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.NonCapture(inner)
  }

  object Cat extends PureParserBridge1[NonEmptyList[ToRegex], ToRegex] {
    override def apply(regexes: NonEmptyList[ToRegex]): ToRegex = {
      val NonEmptyList(head, tail) = regexes
      tail.foldLeft(head)(ast.Cat(_, _))
    }
  }

  object Alt extends PureParserBridge2[ToRegex, ToRegex, ToRegex] {
    override def apply(left: ToRegex, right: ToRegex): ToRegex = ast.Alt(left, right)
  }

  object Opt extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Opt(inner)
  }

  object Star extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Star(inner)
  }

  object Plus extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Plus(inner)
  }

  private inline def ast(using ast: AST): ast.type = ast
}
