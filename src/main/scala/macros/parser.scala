package experiments.macros

import experiments.macros.ast.AST
import experiments.macros.bridges.{Alt, Capture, Cat, Dot, Lit, NonCapture, Opt, Star, Plus, ToRegex}
import parsley.{Parsley, Result}
import parsley.cats.combinator.some
import parsley.expr.chain
import parsley.quick.{atomic, eof, noneOf}
import parsley.syntax.character.{charLift, stringLift}
import scala.quoted.Quotes

object parser {

  def parse(s: String, ast: AST)(using q: Quotes): Result[String, ast.Regex[?]] = regex.parse(s).map(_(using ast, q))

  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[ToRegex] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(some(atomWithPostfix))
  private lazy val atomWithPostfix = chain.postfix(atom)(postfixOps)
  private lazy val atom = nonCapture | capture | lit | dot

  private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
  private lazy val capture = Capture('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars).map(_.toInt))
  private lazy val dot = (Dot from '.')

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

  private lazy val postfixOps = (Opt from '?') <|> (Star from '*') <|> (Plus from '+')
}
