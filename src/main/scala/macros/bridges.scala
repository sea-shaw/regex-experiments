package experiments.macros

import cats.collections.Diet
import experiments.macros.ast
import experiments.macros.ast.Regex
import parsley.templates.{PureParserBridge1, PureParserBridge2}

object bridges {

  object Alt extends PureParserBridge2[Regex[?], Regex[?], ast.Alt[?, ?]] {
    override def apply(left: Regex[?], right: Regex[?]) = ast.Alt(left, right)
  }

  object Capture extends PureParserBridge1[Regex[?], ast.Capture[?]] {
    override def apply(inner: Regex[?]): ast.Capture[?] = ast.Capture(inner)
  }

  object Cat extends PureParserBridge2[Regex[?], List[Regex[?]], Regex[?]] {
    // TODO: This must be a fold, I can't figure out which
    override def apply(head: Regex[?], tail: List[Regex[?]]): Regex[?] = {
      tail.foldLeft(head)(ast.Cat(_, _))
    }
  }

  object Lit extends PureParserBridge1[Int, ast.Lit] {
    override def apply(c: Int): ast.Lit = ast.Lit(c)
  }

  object Class extends PureParserBridge1[Diet[Int], ast.Class] {
    override def apply(cs: Diet[Int]): ast.Class = ast.Class(cs)
  }

  object NonCapture extends PureParserBridge1[Regex[?], ast.NonCapture[?]] {
    override def apply(inner: Regex[?]): ast.NonCapture[?] = ast.NonCapture(inner)
  }

  object Opt extends PureParserBridge1[Regex[?], ast.Opt[?]] {
    override def apply(inner: Regex[?]): ast.Opt[?] = ast.Opt(inner)
  }

  object Rep0 extends PureParserBridge1[Regex[?], ast.Rep0[?]] {
    override def apply(inner: Regex[?]): ast.Rep0[?] = ast.Rep0(inner)
  }

  object Rep1 extends PureParserBridge1[Regex[?], ast.Rep1[?]] {
    override def apply(inner: Regex[?]): ast.Rep1[?] = ast.Rep1(inner)
  }
}
