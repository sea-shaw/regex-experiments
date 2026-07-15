package experiments.macros

import cats.data.ValidatedNec
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all.*
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.quoted.runtime.QuoteMatching
import scala.annotation.tailrec

object matching {

  inline def resolveMatchType[A, F[_], G[_]](inline cases: Any): G[F[A]] = ${ resolveMatchTypeCode[A, F, G]('cases) }
  private def resolveMatchTypeCode[A: Type, F[_]: Type, G[_]: Type](expr: Expr[?])(using Quotes): Expr[G[F[A]]] = {
    import quotes.reflect.*

    case class CompileError(msg: String, pos: Position)
    case class TypeReprAndPos(tpe: TypeRepr, pos: Position)

    val builder = StringBuilder()

    @tailrec
    def finalTerm(term: Term): Term = term match {
      case Inlined(call, bindings, expansion) => {
        builder ++= s"# Inlined\n\nCall: ${call.map(_.show)}\n\nBindings: ${bindings.map(_.show)}\n\n"
        finalTerm(expansion)
      }
      case Block(_, term)                     => finalTerm(term)
      case Typed(term, _)                     => finalTerm(term)
      case _                                  => term 
    }

    def getPatternType(pattern: Tree, selectorIdent: String): ValidatedNec[CompileError, TypeReprAndPos] = {
      pattern match {
        case Unapply(TypeApply(Select(Select(TypeApply(Select(Ident(selectorIdent2), "asInstanceOf"), List(quoteMatching)), "TypeMatch"), "unapply"), _), List(Apply(TypeApply(Ident("of"), List(tpe)), List(Ident(selectorIdent3)))), _) if selectorIdent2 == selectorIdent && selectorIdent3 == selectorIdent && quoteMatching.tpe =:= TypeRepr.of[QuoteMatching] => {
          TypeReprAndPos(tpe.tpe, tpe.pos).validNec
        }
        case _ => CompileError(s"Invalid pattern ${pattern.show}. Pattern must be of the form `'[a]`.", pattern.pos).invalidNec
      }
    }

    // TODO: What aboud nested patterns, e.g. `F[G[a]]`?
    def typeVariables(pattern: TypeRepr): List[TypeRepr] = pattern.typeArgs.filter(_.typeSymbol.isAliasType)

    lazy val notMatchTypeError = CompileError(s"Not a match type", TypeTree.of[F].pos).invalidNec

    val optionMatchType = TypeRepr.of[F].dealias match {
      case typeRef: TypeRef => typeRef.translucentSuperType match {
        case lambdaType: LambdaType => lambdaType.resType match {
          case matchType: MatchType => matchType.validNec
          case _                    => notMatchTypeError
        }
        case _                      => notMatchTypeError
      }
      case _                => notMatchTypeError
    }

    optionMatchType.foreach { matchType =>
      builder ++= s"# Match Type\n\nBound: ${matchType.bound.show}\n\nScrutinee: ${matchType.scrutinee.show}\n\nCases: ${matchType.cases.map(_.show)}\n\n"
    }

    val lastTerm = finalTerm(expr.asTerm)
    builder ++= s"# Final Term\n\n${lastTerm.show}\n\n"

    val optionMatchTerm = lastTerm match {
      case Match(selector, cases) => selector match {
        case Apply(TypeApply(Select(Ident("Type"),"of"), List(tpe)), List(Ident(name))) => {
          report.info(tpe.tpe.show, tpe.pos)
          (name, tpe).validNec
        }
        case _ => {
          CompileError("Selector must be of the form `Type.of[A]` where `A` is the scrutinee of the match types.", selector.pos).invalidNec
        }
      } andThen { case (selectorIdent, selectorType) =>
        cases.map { c =>
          c match {
            case CaseDef(_, Some(guard), _) => {
              CompileError("Pattern cannot have guards", guard.pos).invalidNec
            }
            case CaseDef(pattern, None, rhs) => {
              getPatternType(pattern, selectorIdent).map((_, rhs))
            }
          }
        }.sequence.map((selectorType, _))
      }
    }

    val res = (optionMatchType, optionMatchTerm).mapN { case (MatchType(_, _, typeCases), (_, termCases)) =>
      typeCases.zip(termCases).map { case (typeCase, (termPattern, termRhs)) =>
        val (typePattern, typeRhs) = typeCase match {
          case MatchCase(typePattern, typeRhs) => {
            (typePattern, typeRhs)
          }
          case typeLambda: TypeLambda => typeLambda.appliedTo(typeVariables(termPattern.tpe)) match {
            case MatchCase(typePattern, typeRhs) => (typePattern, typeRhs)
          }
        }
        report.info(s"Type: ${typePattern.show}\n\nTerm: ${termPattern.tpe.show}\n\nEqual: ${typePattern =:= termPattern.tpe}", termPattern.pos)
        report.info(typeRhs.show, termRhs.pos)
      }
    }

    report.info(builder.toString, Position.ofMacroExpansion)

    res match {
      case Valid(_) => '{ ??? }
      case Invalid(errors) => { 
        val (init, last) = errors.initLast
        init.map { case CompileError(msg, pos) =>
          report.error(msg, pos)
        }
        report.errorAndAbort(last.msg, last.pos)
      }
    }
  }
}
