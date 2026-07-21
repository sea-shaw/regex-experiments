package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.annotation.tailrec

object tidy {
  def tidyType[A <: HChain: Type](using Quotes): Type[?] = {
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
      case '[HAppend[a, b]] => {
        // TODO: How to get rid of `& HChain`
        // `a <: HChain`
        build[Acc](Type.of[b & HChain], Type.of[a & HChain] :: types)
      }
    }

    build[EmptyTuple](Type.of[A], Nil)
  }

  def printTidyTypeCode[A <: HChain: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.TypeRepr
    val tidied = Expr(TypeRepr.of(using tidyType[A]).show)
    '{ println($tidied) }
  }

  inline def printTidyType[A <: HChain]: Unit = ${ printTidyTypeCode[A] }
}
