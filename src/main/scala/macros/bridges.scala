package experiments.macros

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
    override def apply(head: Regex[?], tail: List[Regex[?]]): Regex[?] = tail match {
      case Nil => head
      case r :: rs => ast.Cat(head, Cat(r, rs))
    }
  }

  object Lit extends PureParserBridge1[Char, ast.Lit] {
    override def apply(c: Char): ast.Lit = ast.Lit(c)
  }

  object NonCapture extends PureParserBridge1[Regex[?], ast.NonCapture[?]] {
    override def apply(inner: Regex[?]): ast.NonCapture[?] = ast.NonCapture(inner)
  }

  object Opt extends PureParserBridge1[Regex[?], ast.Opt[?]] {
    override def apply(inner: Regex[?]): ast.Opt[?] = ast.Opt(inner)
  }

  object Many extends PureParserBridge1[Regex[?], ast.Many[?]] {
    override def apply(inner: Regex[?]): ast.Many[?] = ast.Many(inner)
  }
}
