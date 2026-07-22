package experiments.macros

import cats.data.Chain
import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}

object tidy {
  case class ElemFunction[A <: HChain, B](elem: Expr[A] => Expr[B], tpe: Type[B])
  case class TidyFunction[A <: HChain, B](tidy: Expr[A] => Expr[B], tpe: Type[B])

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    tidyFunction[A].tidy(xs)
  }

  // TODO: Can this be tail-recursive?
  def elemFunctions[A <: HChain: Type](using Quotes): Chain[ElemFunction[A, ?]] = Type.of[A] match {
    case '[HEmpty] => Chain.nil
    case '[HSingleton[a]] => {
      val elem = { (x: Expr[A]) =>
        val singleton = x.asExprOf[HSingleton[a]]
        '{ $singleton.value }
      }
      Chain.one(ElemFunction(elem, Type.of[a]))
    }
    case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
      val leftFunctions = elemFunctions[a].map { case ElemFunction(leftElem, tpe) => 
        val elem = { (x: Expr[A]) =>
          val append = x.asExprOf[HAppend[a, b]]
          leftElem('{ $append.left })
        }
        ElemFunction(elem, tpe)
      }
      val rightFunctions = elemFunctions[b].map { case ElemFunction(rightElem, tpe) => 
        val elem = { (x: Expr[A]) =>
          val append = x.asExprOf[HAppend[a, b]]
          rightElem('{ $append.right })
        }
        ElemFunction(elem, tpe)
      }
      leftFunctions ++ rightFunctions
    }
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {
    import quotes.reflect.{Position, report}

    val vec = elemFunctions[A].toVector
    vec match {
      case Vector() => {
        TidyFunction(_ => '{ () }, Type.of[Unit])
      }
      case Vector(ElemFunction(e0, t0)) => {
        TidyFunction(e0, Type.of[t0.Underlying])
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1)) => {
        given tpe0: Type[t0.Underlying] = t0
        given tpe1: Type[t1.Underlying] = t1

        val tidy = (xs: Expr[A]) => '{ (${e0(xs)}, ${e1(xs)}) }
        val tpe = Type.of[(t0.Underlying, t1.Underlying)]
        TidyFunction(tidy, tpe)
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1), ElemFunction(e2, t2)) => {
        given type0: Type[t0.Underlying] = t0
        given type1: Type[t1.Underlying] = t1
        given type2: Type[t2.Underlying] = t2

        val tidy = (xs: Expr[A]) => '{ (${e0(xs)}, ${e1(xs)}, ${e2(xs)}) }
        val tpe = Type.of[(t0.Underlying, t1.Underlying, t2.Underlying)]
        TidyFunction(tidy, tpe)
      }
      case Vector(ElemFunction(e0, t0), ElemFunction(e1, t1), ElemFunction(e2, t2), ElemFunction(e3, t3)) => {
        given type0: Type[t0.Underlying] = t0
        given type1: Type[t1.Underlying] = t1
        given type2: Type[t2.Underlying] = t2
        given type3: Type[t3.Underlying] = t3

        val tidy = (xs: Expr[A]) => '{ (${e0(xs)}, ${e1(xs)}, ${e2(xs)}, ${e3(xs)}) }
        val tpe = Type.of[(t0.Underlying, t1.Underlying, t2.Underlying, t3.Underlying)]
        TidyFunction(tidy, tpe)
      }
      // TODO: 18 more cases. What about tuples with >22 elements?
      case _ => report.errorAndAbort(s"Unsupported tuple size ${vec.size}", Position.ofMacroExpansion)
    }
  }
}
