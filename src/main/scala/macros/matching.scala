package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}
import cats.syntax.all.*
import scala.quoted.runtime.QuoteMatching
import scala.annotation.tailrec

object matching {

  inline def resolveMatchType[A, F[_], G[_]](inline cases: Any): G[F[A]] = ${ resolveMatchTypeCode[A, F, G]('cases) }
  private def resolveMatchTypeCode[A: Type, F[_]: Type, G[_]: Type](expr: Expr[?])(using Quotes): Expr[G[F[A]]] = {
    import quotes.reflect.*

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

    def getPatternType(pattern: Tree, selectorIdent: String): Option[TypeTree] = {
      pattern match {
        case Unapply(TypeApply(Select(Select(TypeApply(Select(Ident(selectorIdent2), "asInstanceOf"), List(quoteMatching)), "TypeMatch"), "unapply"), _), List(Apply(TypeApply(Ident("of"), List(tpe)), List(Ident(selectorIdent3)))), _) if selectorIdent2 == selectorIdent && selectorIdent3 == selectorIdent && quoteMatching.tpe =:= TypeRepr.of[QuoteMatching] => {
          // report.info(s"${tpe.tpe.show}", tpe.pos)
          Some(tpe)
        }
        case _ => {
          report.error(s"Invalid pattern ${pattern.show}. Pattern must be of the form `'[a]`.", pattern.pos)
          None
        }
      }
    }

    val optionMatchType = TypeRepr.of[F].dealias match {
      case typeRef: TypeRef => typeRef.translucentSuperType match {
        case lambdaType: LambdaType => lambdaType.resType match {
          case matchType: MatchType => Some(matchType)
          case _                    => None
        }
        case _                      => None
      }
      case _                => None
    }

    optionMatchType.foreach { matchType =>
      builder ++= s"# Match Type\n\nBound: ${matchType.bound.show}\n\nScrutinee: ${matchType.scrutinee.show}\n\nCases: ${matchType.cases.map(_.show)}\n\n"
    }

    val lastTerm = finalTerm(expr.asTerm)
    builder ++= s"# Final Term\n\n${lastTerm.show}\n\n"

    val optionMatchTerm = lastTerm match {
      case Match(selector, cases) => for {
        (selectorIdent, selectorType) <- selector match {
          case Apply(TypeApply(Select(Ident("Type"),"of"), List(tpe)), List(Ident(name))) => {
            report.info(tpe.tpe.show, tpe.pos)
            Some((name, tpe))
          }
          case _ => {
            report.error("Selector must be of the form `Type.of[A]` where `A` is the scrutinee of the match types.", selector.pos)
            None
          }
        }
        caseComponents <- cases.map { c =>
          c match {
            case CaseDef(_, Some(guard), _) => {
              report.error("Pattern cannot have guards", guard.pos)
              None
            }
            case CaseDef(pattern, None, rhs) => {
              getPatternType(pattern, selectorIdent).map((_, rhs))
            }
          }
        }.sequence
      } yield (selectorType, caseComponents)
      case _ => None
    }

    // optionMatchTerm.foreach { case (selectorType, caseComponents) =>
    //   report.info(selectorType.show, selectorType.pos)
    //   caseComponents.foreach { case (typePattern, rhs) =>
    //     // report.info(typePattern.show, typePattern.pos)
    //     // report.info(rhs.show, rhs.pos)
    //   }
    // }

    (optionMatchType, optionMatchTerm).mapN { case (MatchType(_, _, typeCases), (_, termCases)) =>
      typeCases.zip(termCases).map { case (typeCase, (termPattern, termRhs)) =>
        val (typePattern, typeRhs) = typeCase match {
          case MatchCase(typePattern, typeRhs) => {
            (typePattern, typeRhs)
          }
          case typeLambda: TypeLambda => typeLambda.appliedTo(termPattern.tpe.typeArgs) match {
            case MatchCase(typePattern, typeRhs) => (typePattern, typeRhs)
          }
        }
        report.info(s"Type: ${typePattern.show}\n\nTerm: ${termPattern.tpe.show}\n\nEqual: ${typePattern =:= termPattern.tpe}", termPattern.pos)
        report.info(typeRhs.show, termRhs.pos)
      }
    }

    report.info(builder.toString, Position.ofMacroExpansion)

    '{ ??? }
  }
}
