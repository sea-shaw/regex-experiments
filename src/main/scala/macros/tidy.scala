package experiments.macros

import cats.data.{Chain, Ior}
import cats.syntax.all.*
import experiments.macros.evidence.<:<.{apply}
import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty, HNonEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.annotation.unused

object tidy {
  class ElemFunction[A <: HChain, B](val elem: Expr[A] => Quotes ?=> Expr[B], val tpe: Type[B])
  class TidyFunction[A <: HChain, B](val tidy: Expr[A] => Quotes ?=> Expr[B], val tpe: Type[B])

  object ElemFunction {
    def unapply[A <: HChain, B](elemFunction: ElemFunction[A, B]): (Expr[A] => Quotes ?=> Expr[B], Type[B]) = {
      (elemFunction.elem, elemFunction.tpe)
    }
  }

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    tidyFunction[A].tidy(xs)
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {
    import quotes.reflect.{Position, report}

    val vec = elemFunctions[A].toVector
    vec match {
      case Vector() => {
        def tidy(@unused expr: Expr[A])(using Quotes) = '{ () }
        TidyFunction(tidy, Type.of[Unit])
      }
      case Vector(ElemFunction(e0, t0)) => {
        def tidy(xs: Expr[A])(using Quotes) = e0(xs)
        TidyFunction(tidy, Type.of[t0.Underlying])
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1)) => {
        type T0 = t0.Underlying
        type T1 = t1.Underlying

        given Type[T0] = t0
        given Type[T1] = t1

        def tidy(xs: Expr[A])(using Quotes) = '{ (${e0(xs)}, ${e1(xs)}) }
        val tpe = Type.of[(T0, T1)]
        TidyFunction(tidy, tpe)
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1), ElemFunction(e2, t2)) => {
        type T0 = t0.Underlying
        type T1 = t1.Underlying
        type T2 = t2.Underlying

        given Type[T0] = t0
        given Type[T1] = t1
        given Type[T2] = t2

        def tidy(xs: Expr[A])(using Quotes) = '{ (${e0(xs)}, ${e1(xs)}, ${e2(xs)}) }
        val tpe = Type.of[(T0, T1, T2)]
        TidyFunction(tidy, tpe)
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1), ElemFunction(e2, t2), ElemFunction(e3, t3)) => {
        type T0 = t0.Underlying
        type T1 = t1.Underlying
        type T2 = t2.Underlying
        type T3 = t3.Underlying

        given Type[T0] = t0
        given Type[T1] = t1
        given Type[T2] = t2
        given Type[T3] = t3

        def tidy(xs: Expr[A])(using Quotes) = '{ (${e0(xs)}, ${e1(xs)}, ${e2(xs)}, ${e3(xs)}) }
        val tpe = Type.of[(T0, T1, T2, T3)]
        TidyFunction(tidy, tpe)
      }
      // TODO: 18 more cases. What about tuples with >22 elements?
      case _ => report.errorAndAbort(s"Unsupported tuple size ${vec.size}", Position.ofMacroExpansion)
    }
  }

  // TODO: Can this be tail-recursive?
  private def elemFunctions[A <: HChain: Type](using Quotes): Chain[ElemFunction[A, ?]] = {
    val fs: Option[Chain[ElemFunction[A, ?]]] = Type.of[A] match {
      case '[HEmpty] => Some(Chain.nil)
      case '[type a <: HChain; HSingleton[Option[a]]] => Expr.summon[A <:< HSingleton[Option[a]]].map { ev =>
        // Needs type annotation otherwise we get a cyclic reference
        val inner: TidyFunction[a, ?] = tidyFunction[a]
        val tidy = inner.tidy

        type B = inner.tpe.Underlying
        given Type[B] = inner.tpe

        def elem(expr: Expr[A])(using Quotes) = { 
          val singleton = ev(expr)
          '{ $singleton.value.map(value => ${ tidy('value) }) }
        }

        Chain.one(ElemFunction(elem, Type.of[Option[B]]))
      }
      case '[type a <: HChain; type b <: HChain; HSingleton[Either[a, b]]] => Expr.summon[A <:< HSingleton[Either[a, b]]].map { ev =>
        val left: TidyFunction[a, ?] = tidyFunction[a]
        val right: TidyFunction[b, ?] = tidyFunction[b]

        type L = left.tpe.Underlying
        type R = right.tpe.Underlying 
        given Type[L] = left.tpe
        given Type[R] = right.tpe

        def elem(expr: Expr[A])(using Quotes) = {
          val singleton = ev(expr)
          '{
            $singleton.value.bimap(
              value => ${ left.tidy('value) },
              value => ${ right.tidy('value) }
            )
          }
        }

        Chain.one(ElemFunction(elem, Type.of[Either[L, R]]))
      }
      case '[type a <: HChain; type b <: HChain; HSingleton[Ior[a, b]]] => Expr.summon[A <:< HSingleton[Ior[a, b]]].map { ev =>
        val left: TidyFunction[a, ?] = tidyFunction[a]
        val right: TidyFunction[b, ?] = tidyFunction[b]

        type L = left.tpe.Underlying
        type R = right.tpe.Underlying
        given Type[L] = left.tpe
        given Type[R] = right.tpe

        def elem(expr: Expr[A])(using Quotes) = {
          val singleton = ev(expr)
          '{
            $singleton.value.bimap(
              value => ${ left.tidy('value) },
              value => ${ right.tidy('value) }
            )
          }
        }

        val tpe = Type.of[Ior[L, R]]
        Chain.one(ElemFunction(elem, tpe))
      }
      case '[HSingleton[a]] => Expr.summon[A <:< HSingleton[a]].map { ev =>
        def elem(expr: Expr[A])(using Quotes) = {
          val singleton = ev(expr)
          '{ $singleton.value }
        }
        Chain.one(ElemFunction(elem, Type.of[a]))
      }
      case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => Expr.summon[A <:< HAppend[a, b]].map { ev =>
        val leftFunctions = elemFunctions[a].map { case ElemFunction(leftElem, tpe) => 
          def elem(expr: Expr[A])(using Quotes) = {
            val append = ev(expr)
            leftElem('{ $append.left })
          }
          ElemFunction(elem, tpe)
        }
        val rightFunctions = elemFunctions[b].map { case ElemFunction(rightElem, tpe) => 
          def elem(expr: Expr[A])(using Quotes) = {
            val append = ev(expr)
            rightElem('{ $append.right })
          }
          ElemFunction(elem, tpe)
        }
        leftFunctions ++ rightFunctions
      }
    }

    fs.get
  }

  transparent inline def tidy2[A <: HChain](xs: A): Any = ${ tidy2Code('xs) }

  case class TypedNode[A <: HNonEmpty](node: Expr[A], tpe: Type[A])
  case class TypedLeaf[A](leaf: Expr[A], tpe: Type[A])

  private def tidy2Code[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = Type.of[A] match {
    case '[HEmpty] => '{ () }
    case '[HSingleton[a]] => tidyNonEmpty(xs.asExprOf[HSingleton[a]])
    case '[HAppend[a, b]] => tidyNonEmpty(xs.asExprOf[HAppend[a, b]])
  }

  private def tidyNonEmpty[A <: HNonEmpty: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    def go(nodes: List[TypedNode[?]], leaves: List[TypedLeaf[?]])(using Quotes): Expr[?] = nodes match {
      case Nil     => build(leaves)
      case TypedNode(node, tpe) :: ns => tpe match {
        case '[HSingleton[a]] => {
          val singleton = node.asExprOf[HSingleton[a]]
          go(ns, TypedLeaf('{ $singleton.value }, Type.of[a]) :: leaves)
        }
        case '[type a <: HNonEmpty; type b <: HNonEmpty; HAppend[a, b]] => {
          val append = node.asExprOf[HAppend[a, b]]
          '{
            val left = $append.left
            val right = $append.right
            ${ go(TypedNode('{ right }, Type.of[b]) :: TypedNode('{ left }, Type.of[a]) :: ns, leaves) }
          }
        }
      }
    }

    go(TypedNode(xs, Type.of[A]) :: Nil, Nil)
  }

  private def build(leaves: List[TypedLeaf[?]])(using Quotes): Expr[?] = {
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
}

