package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.quoted.runtime.QuoteMatching
import scala.annotation.tailrec

object matching {
  object QuotePattern {
    def unapply(using q: Quotes)(tree: q.reflect.Tree): Option[(List[?], q.reflect.Ident, q.reflect.Ident)] = {
      import quotes.reflect.*
      ???
    }
  }

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
                    tpe.asType match {
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

  private def getPatternType(using q: Quotes)(pattern: q.reflect.Tree, selectorIdent: String): Option[q.reflect.TypeRepr] = {
    import quotes.reflect.*
    pattern match {
      case Unapply(TypeApply(Select(Select(TypeApply(Select(Ident(selectorIdent2), "asInstanceOf"), List(quoteMatching)), "TypeMatch"), "unapply"), _), List(Apply(TypeApply(Ident("of"), List(tpe)), List(Ident(selectorIdent3)))), _) if selectorIdent2 == selectorIdent && selectorIdent3 == selectorIdent && quoteMatching.tpe =:= TypeRepr.of[QuoteMatching] => {
        report.info(s"${tpe.tpe.show}", tpe.pos)
        Some(tpe.tpe)
      }
      case _ => {
        report.error(s"Invalid pattern ${pattern.show}", pattern.pos)
        None
      }
    }
  }

  /*
  access to parameter x$2 from wrong staging level:
  - the definition is at level 0,
  - but the access is at level 1.

  Hint: Nested quote needs a local context defined at level 1.
  One way to introduce this context is to give the outer quote the type `Expr[Quotes ?=> Expr[T]]`.
  */

  // inline def debugTypeMatch2[A](inline a: Quotes ?=> A)(using q: Quotes): (Quotes ?=> A) = ${ debugTypeMatch2Code('a) }
  // private def debugTypeMatch2Code[A: Type](expr: Expr[Quotes ?=> A])(using Quotes): Expr[Quotes ?=> A] = ???

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
  Inlined(
    EmptyTree
    List()
    Typed(
      Block(
        List()
        Match(
          Apply(
            TypeApply(
              Select(Ident(Type),of),
              List(Ident(A))
            ),
            List(Ident(x$1))
          ),
          List(
            CaseDef(
              QuotePattern(
                List(),
                Ident(String),
                Ident(x$1)
              ),
              EmptyTree,
              Block(
                List(),
                Ident(???)
              )
            )
          )
        )
      ),
      TypeTree[AnnotatedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Nothing),ConcreteAnnotation(Apply(Select(New(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class internal)),class InlineParam)]),<init>),List())))]))
  */

  /* Inlined(EmptyTree,List(),Typed(Block(List(),Match(Apply(TypeApply(Select(Ident(Type),of),List(Ident(A))),List(Ident(x$1))),List(CaseDef(QuotePattern(List(),Ident(String),Ident(x$1)),EmptyTree,Block(List(),Ident(???)))))),TypeTree[AnnotatedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Nothing),ConcreteAnnotation(Apply(Select(New(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class internal)),class InlineParam)]),<init>),List())))])) */
}
