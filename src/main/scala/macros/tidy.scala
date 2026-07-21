package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty, toList}
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.annotation.tailrec

object tidy {

  inline def printTidyType[A <: HChain]: Unit = ${ printTidyTypeCode[A] }

  def printTidyTypeCode[A <: HChain: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.TypeRepr
    val tidied = Expr(TypeRepr.of(using tidyType[A]).show)
    '{ println($tidied) }
  }

  transparent inline def tidy[A <: HChain](xs: A): Tuple = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[Tuple] = {
    tidyType[A] match {
      case '[type a <: Tuple; a] => '{ Tuple.fromArray($xs.toList.toArray).asInstanceOf[a] }
    }
  }

  private def tidyType[A <: HChain: Type](using Quotes): Type[?] = {
    @tailrec
    def build[Acc <: Tuple: Type](tpe: Type[? <: HChain], types: List[Type[? <: HChain]]): Type[? <: Tuple] = tpe match {
      case '[HEmpty] => types match {
        case tpe :: types => build[Acc](tpe, types)
        case Nil          => Type.of[Acc]
      }
      case '[HSingleton[a]] => types match {
        case tpe :: types => build[a *: Acc](tpe, types)
        case Nil          => Type.of[a *: Acc]
      }
      case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
        build[Acc](Type.of[b], Type.of[a] :: types)
      }
    }

    build[EmptyTuple](Type.of[A], Nil)
  }
}
