package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}

object tidy {
  class TidyFunction[A <: HChain, B](val tidy: Expr[A] => Quotes ?=> Expr[B], val tpe: Type[B])

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = ???

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {

    case class TypedNode[A <: HChain](node: Expr[A], tpe: Type[A])
    case class TypedLeaf[A](leaf: Expr[A], tpe: Type[A])

    def go(nodes: List[TypedNode[?]], leaves: List[TypedLeaf[?]])(using Quotes): Expr[?] = nodes match {
      case Nil     => build(leaves)
      case TypedNode(node, tpe) :: ns => tpe match {
        case '[HEmpty] => go(ns, leaves)
        case '[HSingleton[a]] => {
          val singleton = node.asExprOf[HSingleton[a]]
          go(ns, TypedLeaf('{ $singleton.value }, Type.of[a]) :: leaves)
        }
        case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
          val append = node.asExprOf[HAppend[a, b]]
          '{
            val left = $append.left
            val right = $append.right
            ${ go(TypedNode('{ right }, Type.of[b]) :: TypedNode('{ left }, Type.of[a]) :: ns, leaves) }
          }
        }
      }
    }

    def build(leaves: List[TypedLeaf[?]])(using Quotes): Expr[?] = {
      import quotes.reflect.{Position, report}
      val elems = leaves.toVector

      elems match {
        case Vector() => '{ () }
        case Vector(TypedLeaf(e0, _)) => e0
        case Vector(TypedLeaf(e0, t0), TypedLeaf(e1, t1)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying

          given Type[T0] = t0
          given Type[T1] = t1

          '{ ($e0, $e1) }
        }
        case Vector(TypedLeaf(e0, t0), TypedLeaf(e1, t1), TypedLeaf(e2, t2)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying

          given Type[T0] = t0
          given Type[T1] = t1
          given Type[T2] = t2

          '{ ($e0, $e1, $e2) }
        }
        case Vector(TypedLeaf(e0, t0), TypedLeaf(e1, t1), TypedLeaf(e2, t2), TypedLeaf(e3, t3)) => {
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

    go(TypedNode(xs, Type.of[A]) :: Nil, Nil)
  }
}

