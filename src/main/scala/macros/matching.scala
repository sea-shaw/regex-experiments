package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}
import cats.syntax.all.*
import scala.quoted.runtime.QuoteMatching
import scala.annotation.tailrec

object matching {

  inline def debugTree[A](inline x: A): A = ${ debugTreeCode('x) }
  private def debugTreeCode[A: Type](x: Expr[A])(using Quotes): Expr[A] = {
    import quotes.reflect.{Position, asTerm, report}
    val term = x.asTerm
    val expr = term.asExprOf[A]
    report.info(term.toString(), Position.ofMacroExpansion)
    expr
  }

  inline def debugTypeMatch[A](inline x: A): A = ${ debugTypeMatchCode('x) }
  private def debugTypeMatchCode[A: Type](x: Expr[A])(using Quotes): Expr[A] = {
    import quotes.reflect.*
    x.asTerm match {
      case Inlined(_, _, expansion) => {
        finalTerm(expansion) match {
          case Match(selector, cases) => {
            val selectorIdent = selector match {
              case Apply(TypeApply(Select(Ident("Type"),"of"), List(tpe)), List(Ident(name))) => {
                report.info(tpe.tpe.show, tpe.pos)
                name
              }
              case _                                                                          => report.errorAndAbort("invaid", selector.pos)
            }
            cases.foreach { c =>
              c match {
                case CaseDef(_, Some(guard), _) => {
                  report.error("Pattern cannot have guards", guard.pos)
                }
                case CaseDef(pattern, None, rhs) => {
                  getPatternType(pattern, selectorIdent).map { tpe =>
                    tpe.tpe.asType match {
                      case '[a] => if (!rhs.isExpr || !rhs.asExpr.isExprOf[a]) {
                        report.error(s"Expected ${tpe.show}", rhs.pos)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    x
  }

  private def getPatternType(using q: Quotes)(pattern: q.reflect.Tree, selectorIdent: String): Option[q.reflect.TypeTree] = {
    import quotes.reflect.*
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

  @tailrec
  private def finalTerm(using q: Quotes)(term: q.reflect.Term): q.reflect.Term = {
    import quotes.reflect.{Block, Typed}
    term match {
      case Typed(expr, _) => finalTerm(expr)
      case Block(_, expr) => finalTerm(expr)
      case expr           => expr
    }
  }

  /*
  TypeDef("ElemType", LambdaTypeTree(List(TypeDef("A", TypeBoundsTree(Inferred(), Inferred()))), MatchTypeTree(None, TypeIdent("A"), List(TypeCaseDef(TypeIdent("String"), TypeIdent("Char")), TypeCaseDef(Applied(TypeIdent("Array"), List(TypeBind(a, Wildcard()))), Applied(TypeIdent("ElemType"), List(TypeIdent("a")))), TypeCaseDef(Applied(TypeIdent("Iterable"), List(TypeBind(a, Wildcard()))), Applied(TypeIdent("ElemType"), List(TypeIdent("a")))), TypeCaseDef(TypeIdent("_"), TypeIdent("A"))))))
  */

  inline def resolveMatchType[A, F[_ <: A]](inline cases: Any): F[A]  = ${ debugMatchTypeCode[A, F]('cases) }
  private def debugMatchTypeCode[A: Type, F[_ <: A]: Type](expr: Expr[?])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.{TypeRepr, report}
    (matchTypeComponents[A, F], matchComponents(expr)).mapN { case (typePatterns, (exprSelector, exprCases)) =>
      if (!(exprSelector.tpe <:< TypeRepr.of[A])) {
        report.error("Match expression scrutinee must be a subtype of `A`.", exprSelector.pos)
      }
      typePatterns.zip(exprCases).foreach { case ((typeCase, rhsType), (exprCase, term)) =>
        val typeCaseAliases = typeCase.typeArgs.map(_.typeSymbol).filter(_.isAliasType)
        val exprCaseAliases = exprCase.tpe.typeArgs.filter(_.typeSymbol.isAliasType)
        // report.info(exprCaseAliases.toString, exprCase.pos)
        if (!(typeCase.substituteTypes(typeCaseAliases, exprCaseAliases) =:= exprCase.tpe)) {
          report.error(s"Expected ${typeCase.show} but was ${exprCase.tpe.show}", exprCase.pos)
        }
        val rhsTypeSubstituted = rhsType.substituteTypes(typeCaseAliases, exprCaseAliases)
        report.info(rhsTypeSubstituted.show, exprCase.pos)
        rhsTypeSubstituted.asType match {
          case '[a] => if (!term.isExpr || !term.asExpr.isExprOf[a]) {
            report.error(s"Expected ${rhsTypeSubstituted.show}", term.pos)
          }
        }
      }
    }
    '{ ??? }
    // expr.asExprOf[F[A]]
  }

  private def matchTypeComponents[A: Type, F[_ <: A]: Type](using q: Quotes): Option[List[(q.reflect.TypeRepr, q.reflect.TypeRepr)]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[F]
    val tree = tpe.dealias.typeSymbol.tree
    tree match {
      case TypeDef(_, LambdaTypeTree(_, MatchTypeTree(bound, selector, cases))) => {
        bound.map { bound => report.info(bound.show, bound.pos) }
        report.info(selector.show, selector.pos)
        val caseTypes = cases.map { case TypeCaseDef(pattern, rhs) =>
          report.info(pattern.show, pattern.pos)
          report.info(rhs.show, rhs.pos)
          // val argPatterns = pattern.tpe.typeArgs.filter(_.typeSymbol.isAliasType)
          // report.info(argPatterns.map(_.show).toString, pattern.pos)
          (pattern.tpe.dealias, rhs.tpe.dealias)
        }
        // Some(bound.map(_.tpe), selector.tpe, caseTypes)
        Some(caseTypes)
      }
      case _ => {
        report.error(s"Type ${tpe.show} is not a match type", Position.ofMacroExpansion)
        None
      }
    }
  }

  private def matchComponents(expr: Expr[?])(using q: Quotes): Option[(q.reflect.TypeTree, List[(q.reflect.TypeTree, q.reflect.Term)])] = {
    import quotes.reflect.*
    val term = expr.asTerm
    term match {
      case Inlined(_, _, expansion) => {
        finalTerm(expansion) match {
          case Match(selector, cases) => for {
            (selectorIdent, selectorType) <- selector match {
              case Apply(TypeApply(Select(Ident("Type"),"of"), List(tpe)), List(Ident(name))) => {
                // report.info(tpe.tpe.show, tpe.pos)
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
          case _ => {
            report.error("Final expression must be a `match` expression.", expansion.pos)
            None
          }
        }
      }
      case _ => {
        report.error("Expression must be inlined.", term.pos)
        None
      }
    }
  }
}
