package experiments.macros

import cats.data.{Validated, ValidatedNec}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all.*
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.quoted.runtime.QuoteMatching
import scala.annotation.tailrec

object matching {

  inline def resolveMatchType[A, F[_ <: A], G[_ <: F[A]]](inline cases: Any): G[F[A]] = ${ resolveMatchTypeCode[A, F, G]('cases) }

  // TODO: Allow for type variables in cases
  private def resolveMatchTypeCode[A: Type, F[_ <: A]: Type, G[_ <: F[A]]: Type](expr: Expr[?])(using Quotes): Expr[G[F[A]]] = {
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

    lazy val notMatchTypeError = CompileError(s"Not a match type", Position.ofMacroExpansion).invalidNec

    val validatedMatchType = TypeRepr.of[F].dealias match {
      case typeRef: TypeRef => typeRef.translucentSuperType match {
        case lambdaType: LambdaType => lambdaType.appliedTo(TypeRepr.of[A]) match {
          case matchType: MatchType => matchType.validNec
          case _                    => notMatchTypeError
        }
        case _                      => notMatchTypeError
      }
      case _                => notMatchTypeError
    }

    validatedMatchType.foreach { matchType =>
      builder ++= s"# Match Type\n\nBound: ${matchType.bound.show}\n\nScrutinee: ${matchType.scrutinee.show}\n\nCases: ${matchType.cases.map(_.show)}\n\n"
    }

    val lastTerm = finalTerm(expr.asTerm)
    builder ++= s"# Final Term\n\n${lastTerm.show}\n\n"

    val validatedMatchTerm = lastTerm match {
      case Match(selector, cases) => selector match {
        case Apply(TypeApply(Select(Ident("Type"),"of"), List(tpe)), List(Ident(name))) => {
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

    val res = (validatedMatchType, validatedMatchTerm).mapN((_, _)).andThen { case (MatchType(_, _, typeCases), (termScrutinee, termCases)) =>
      Validated.condNec(termScrutinee.tpe =:= TypeRepr.of[A], (), CompileError(s"Scrutinee must be ${TypeRepr.of[A].show}", termScrutinee.pos)) *>
      typeCases.zip(termCases).map { case (typeCase, (termPattern, termRhs)) =>
        val typePatternAndRhs = typeCase match {
          case MatchCase(typePattern, typeRhs) => {
            (typePattern, typeRhs).validNec
          }
          case typeLambda: TypeLambda => typeLambda.appliedTo(typeVariables(termPattern.tpe)) match {
            case MatchCase(typePattern, typeRhs) => (typePattern, typeRhs).validNec
            case _ => CompileError(s"Unexpected ${termPattern.tpe.show}", termPattern.pos).invalidNec // TODO: Improve error message
          }
          case _ => CompileError("Error deconstructing match type", termPattern.pos).invalidNec
        }
        typePatternAndRhs.andThen { case (typePattern, typeRhs) =>
          // report.info(s"Type: ${typePattern.show}\n\nTerm: ${termPattern.tpe.show}\n\nEqual: ${typePattern =:= termPattern.tpe}", termPattern.pos)
          // report.info(typeRhs.show, termRhs.pos)
          // report.info(s"Pattern type: ${typeRhs.show}\n\nTerm type: ${termRhs.tpe.show}", termRhs.pos)

          val validPattern = if !(typePattern =:= termPattern.tpe) then CompileError(s"Expected ${typePattern.show} but was ${termPattern.tpe.show}", termPattern.pos).invalidNec else ().validNec
          validPattern *> {
            val expectedType =  TypeRepr.of[G].appliedTo(typeRhs).dealias
            val subtype = termRhs.tpe <:< expectedType
            // report.info(s"Pattern type: ${typeRhs.show}\n\nTerm type: ${termRhs.tpe.show}\n\nSubtype: ${subtype}", termRhs.pos)
            if !(subtype) then CompileError(s"Expected ${expectedType.show} but was ${termRhs.tpe.show}", finalTerm(termRhs).pos).invalidNec else ().validNec
          }
        }
      }.sequence
    }

    // report.info(builder.toString, Position.ofMacroExpansion)

    res match {
      // Can't use `asExprOf[G[F[A]]]` because then the compiler checks it and causes a compilation error at the call site.
      case Valid(_) => expr.asInstanceOf[Expr[G[F[A]]]]
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
