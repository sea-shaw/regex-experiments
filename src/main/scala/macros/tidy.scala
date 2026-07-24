package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}

object tidy {
  class TidyFunction[A <: HChain, B](val tidy: Expr[A] => Quotes ?=> Expr[B], val tpe: Type[B])

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {
    ???
  }

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {

    case class TypedExpr[A](expr: Expr[A], tpe: Type[A])

    def go(nodes: List[TypedExpr[?]], leaves: List[TypedExpr[?]])(using Quotes): Expr[?] = nodes match {
      case Nil     => build(leaves)
      case TypedExpr(node, tpe) :: nodes => tpe match {
        case '[HEmpty] => go(nodes, leaves)
        case '[HSingleton[a]] => {
          val singleton = node.asExprOf[HSingleton[a]]
          go(nodes, TypedExpr('{ $singleton.value }, Type.of[a]) :: leaves)
        }
        case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
          val append = node.asExprOf[HAppend[a, b]]
          '{
            val left = $append.left
            val right = $append.right
            ${ go(TypedExpr('{ right }, Type.of[b]) :: TypedExpr('{ left }, Type.of[a]) :: nodes, leaves) }
          }
        }
      }
    }

    def build(leaves: List[TypedExpr[?]])(using Quotes): Expr[?] = {
      import quotes.reflect.{Position, report}
      val elems = leaves.toVector

      elems match {
        case Vector() => '{ () }
        case Vector(TypedExpr(e0, _)) => e0
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying

          given Type[T0] = t0
          given Type[T1] = t1

          '{ ($e0, $e1) }
        }
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1), TypedExpr(e2, t2)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying

          given Type[T0] = t0
          given Type[T1] = t1
          given Type[T2] = t2

          '{ ($e0, $e1, $e2) }
        }
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1), TypedExpr(e2, t2), TypedExpr(e3, t3)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying
          type T3 = t3.Underlying

          given Type[T0] = t0
          given Type[T1] = t1
          given Type[T2] = t2
          given Type[T3] = t3

          '{ ($e0, $e1, $e2, $e3) }
        }
        case _ => report.errorAndAbort(s"Unsupported tuple size ${elems.size}", Position.ofMacroExpansion)
      }
    }

    go(TypedExpr(xs, Type.of[A]) :: Nil, Nil)
  }
}

