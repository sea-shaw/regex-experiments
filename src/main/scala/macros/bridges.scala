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

  // Not currently supported but would make the bridges look a lot nicer.
  // type ToRegex = (ast: AST) => Quotes ?=> ast.Regex[?]

  abstract class ToRegex {
    def apply(ast: AST)(using Quotes): ast.Regex[?]
  }

  object Alt extends PureParserBridge2[ToRegex, ToRegex, ToRegex] {
    override def apply(left: ToRegex, right: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Alt(left(ast), right(ast))
    }
  }

  object Capture extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Capture(inner(ast))
    }
  }

  object Cat extends PureParserBridge1[NonEmptyList[ToRegex], ToRegex] {
    override def apply(regexes: NonEmptyList[ToRegex]): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = {
        val NonEmptyList(head, tail) = regexes
        tail.foldLeft(head(ast))((left, right) => ast.Cat(left, right(ast)))
      }
    }
  }

  object Dot extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure {
      new ToRegex {
        override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Dot()
      }
    }
  }

  object Lit extends PureParserBridge1[Int, ToRegex] {
    override def apply(c: Int): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Lit(c)
    }
  }

  object Class extends PureParserBridge1[Diet[Int], ToRegex] {
    override def apply(cs: Diet[Int]): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Class(cs)
    }
  }

  object NonCapture extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.NonCapture(inner(ast))
    }
  }

  object Opt extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Opt(inner(ast))
    }
  }

  object Rep0 extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Rep0(inner(ast))
    }
  }

  object Rep1 extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = new ToRegex {
      override def apply(ast: AST)(using Quotes): ast.Regex[?] = ast.Rep1(inner(ast))
    }
  }
}
