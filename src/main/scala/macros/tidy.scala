package experiments.macros

import cats.data.{Chain, Ior}
import cats.syntax.all.*
import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}

object tidy {
  case class ElemFunction[A <: HChain, B](elem: Expr[A => B], tpe: Type[B])
  case class TidyFunction[A <: HChain, B](tidy: Expr[A => B], tpe: Type[B])

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    val tidy = tidyFunction[A].tidy
    Expr.betaReduce('{ $tidy($xs) })
  }

  // TODO: Can this be tail-recursive?
  def elemFunctions[A <: HChain: Type](using Quotes): Chain[ElemFunction[A, ?]] = Type.of[A] match {
    case '[HEmpty] => Chain.nil
    case '[type a <: HChain; HSingleton[Option[a]]] => {
      // Needs type annotation otherwise we get a cyclic reference
      val inner: TidyFunction[a, ?] = tidyFunction[a]
      val tidy = inner.tidy

      type B = inner.tpe.Underlying
      given Type[B] = inner.tpe

      val elem: Expr[HSingleton[Option[a]] => Option[B]] = '{ singleton =>
        singleton.value.map($tidy)
      }
      Chain.one(ElemFunction(elem.asExprOf[A => Option[B]], Type.of[Option[B]]))
    }
    case '[type a <: HChain; type b <: HChain; HSingleton[Either[a, b]]] => {
      val left: TidyFunction[a, ?] = tidyFunction[a]
      val right: TidyFunction[b, ?] = tidyFunction[b]

      type L = left.tpe.Underlying
      type R = right.tpe.Underlying 
      given Type[L] = left.tpe
      given Type[R] = right.tpe

      val elem: Expr[HSingleton[Either[a, b]] => Either[L, R]] = '{ singleton =>
        singleton.value.bimap(${left.tidy}, ${right.tidy})
      }
      val tpe = Type.of[Either[L, R]]
      Chain.one(ElemFunction(elem.asExprOf[A => Either[L, R]], tpe))
    }
    case '[type a <: HChain; type b <: HChain; HSingleton[Ior[a, b]]] => {
      val left: TidyFunction[a, ?] = tidyFunction[a]
      val right: TidyFunction[b, ?] = tidyFunction[b]

      type L = left.tpe.Underlying
      type R = right.tpe.Underlying
      given Type[L] = left.tpe
      given Type[R] = right.tpe

      val elem: Expr[HSingleton[Ior[a, b]] => Ior[L, R]] = '{ singleton =>
        singleton.value.bimap(${left.tidy}, ${right.tidy})
      }
      val tpe = Type.of[Ior[L, R]]
      Chain.one(ElemFunction(elem.asExprOf[A => Ior[L, R]], tpe))
    }
    case '[HSingleton[a]] => {
      val elem: Expr[HSingleton[a] => a] = '{ _.value }
      Chain.one(ElemFunction(elem.asExprOf[A => a], Type.of[a]))
    }
    case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
      val leftFunctions = elemFunctions[a].map { case ElemFunction(leftElem, tpe) => 
        type L = tpe.Underlying
        given Type[L] = tpe

        val elem: Expr[HAppend[a, b] => L] = '{ append =>
          $leftElem(append.left)
        }
        ElemFunction(elem.asExprOf[A => L], tpe)
      }
      val rightFunctions = elemFunctions[b].map { case ElemFunction(rightElem, tpe) => 
        type R = tpe.Underlying
        given Type[R] = tpe

        val elem: Expr[HAppend[a, b] => R] = '{ append =>
          $rightElem(append.right)
        }
        ElemFunction(elem.asExprOf[A => R], tpe)
      }
      leftFunctions ++ rightFunctions
    }
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {
    import quotes.reflect.{Position, report}

    val vec = elemFunctions[A].toVector
    vec match {
      case Vector() => {
        val tidy = '{ (_: A) => () }
        TidyFunction(tidy, Type.of[Unit])
      }
      case Vector(ElemFunction(e0, t0)) => {
        TidyFunction(e0, Type.of[t0.Underlying])
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1)) => {
        type T0 = t0.Underlying
        type T1 = t1.Underlying

        given Type[T0] = t0
        given Type[T1] = t1

        val tidy = '{ (xs: A) => ($e0(xs), $e1(xs)) }
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

        val tidy = '{ (xs: A) => ($e0(xs), $e1(xs), $e2(xs)) }
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

        val tidy = '{ (xs: A) => ($e0(xs), $e1(xs), $e2(xs), $e3(xs)) }
        val tpe = Type.of[(T0, T1, T2, T3)]
        TidyFunction(tidy, tpe)
      }
      // TODO: 18 more cases. What about tuples with >22 elements?
      case _ => report.errorAndAbort(s"Unsupported tuple size ${vec.size}", Position.ofMacroExpansion)
    }
  }
}
