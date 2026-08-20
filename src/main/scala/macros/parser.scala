package experiments.macros

import experiments.macros.ast.AST
import experiments.macros.bridges.{Alt, Capture, Cat, Dot, LineEnd, LineStart, Lit, NonCapture, Opt, Star, Plus, ToRegex}
import parsley.{Parsley, Result}
import parsley.cats.combinator.some
import parsley.combinator.option
import parsley.errors.combinator.*
import parsley.expr.chain
import parsley.quick.{atomic, eof, noneOf}
import parsley.syntax.character.{charLift, stringLift}
import parsley.token.Lexer
import parsley.token.descriptions.{LexicalDesc, NumericDesc}
import scala.quoted.Quotes

object parser {

  def parse(s: String, ast: AST)(using q: Quotes): Result[String, ast.Regex[?]] = regex.parse(s).map(_(using ast, q))

  private val regexDesc = LexicalDesc.plain.copy(
    numericDesc = NumericDesc.plain.copy(
      integerNumbersCanBeOctal = false,
      integerNumbersCanBeHexadecimal = false,
    )
  )

  private val lexer = Lexer(regexDesc)

  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[ToRegex] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(some(atom))
  private lazy val atom = boundary | quantified

  private lazy val boundary = (LineStart from '^') | (LineEnd from '$')

  private lazy val quantified: Parsley[ToRegex] = quantifiable <~> option(postfixOps) map {
    case (regex, None)          => regex
    case (regex, Some(postfix)) => postfix(regex)
  }
  private lazy val quantifiable = nonCapture | capture | lit | dot

  private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
  private lazy val capture = Capture('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars).map(_.toInt))
  private lazy val dot = (Dot from '.')

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

  private lazy val postfixOps = (Opt from '?') | (Star from '*') | (Plus from '+') | numericalQuantifier

  private lazy val numericalQuantifier: Parsley[ToRegex => ToRegex] = braces.mapFilterMsg {
    case (0, None | Some(Some(0))) => Left(Seq("Quanitifer cannot be 0"))
    case (1, None | Some(Some(1))) => Right(identity)
    case (n, None)                 => Right(ast.Exactly(_, n))
    case (0, Some(None))           => Right(ast.Star(_))
    case (n, Some(None))           => Right(ast.AtLeast(_, n))
    case (0, Some(Some(m)))        => Right(ast.AtMost(_, m))
    case (n, Some(Some(m)))        => if n <= m then Right(ast.Between(_, n, m)) else Left(Seq("Upper bound cannot be less than lower bound"))
  }
  private lazy val braces = '{' ~> int <~> option(',' ~> option(int)) <~ '}'
  private lazy val int = lexer.lexeme.natural.decimal32[Int]

  private inline def ast(using ast: AST): ast.type = ast
}
