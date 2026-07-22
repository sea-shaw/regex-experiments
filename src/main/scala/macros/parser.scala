package experiments.macros

import experiments.macros.ast.{Regex}
import experiments.macros.bridges.{Alt, Capture, Cat, Dot, Lit, NonCapture, Opt, Rep0, Rep1}
import parsley.Parsley
import parsley.Parsley.pure
import parsley.cats.combinator.some
import parsley.expr.chain
import parsley.quick.{atomic, eof, noneOf}
import parsley.syntax.character.{charLift, stringLift}
import scala.quoted.{Quotes, quotes}

object parser {

  // TODO: Make `parse` take quotes to avoid re-building parser for each `Regex`.
  class Parser(using Quotes) {
    def parse(s: String) = regex.parse(s).toEither

    private lazy val regex = expr <~ eof
    private lazy val expr: Parsley[Regex[?]] = chain.right1(term)((Alt from '|').map(f => (x, y) => f(x, y, quotes)))
    private lazy val term = Cat(some(atomWithPostfix), pure(quotes))
    private lazy val atomWithPostfix = chain.postfix(atom)(postfixOps)
    private lazy val atom = nonCapture | capture | lit | dot

    private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
    private lazy val capture = Capture('(' ~> expr <~ ')', pure(quotes))
    private lazy val lit = Lit(noneOf(keyChars).map(_.toInt), pure(quotes))
    private lazy val dot = (Dot from '.').map(_(quotes))

    private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

    private lazy val postfixOps: Parsley[Regex[?] => Regex[?]] = (Opt from '?').map(f => x => f(x, quotes)) <|> (Rep0 from '*').map(f => x => f(x, quotes)) <|> (Rep1 from '+').map(f => x => f(x, quotes))
  }
}
