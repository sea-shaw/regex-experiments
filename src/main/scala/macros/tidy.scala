package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty}
import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}
import scala.annotation.tailrec
import cats.data.Ior

object tidy {

  inline def printTidyType[A <: HChain]: Unit = ${ printTidyTypeCode[A] }

  private def printTidyTypeCode[A <: HChain: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.TypeRepr
    
    val tpe = buildTidyType[A]
    val str = TypeRepr.of(using tpe).show
    val strExpr = Expr(str)

    '{ println($strExpr) }
  }

  private def buildTidyType[A <: HChain: Type](using Quotes): Type[?] = {
    @tailrec
    def go[Acc <: Tuple: Type](types: List[Type[? <: HChain]]): Type[? <: Tuple] = types match {
      case Nil => Type.of[Acc]
      case tpe :: types => tpe match {
        case '[HEmpty] => go[Acc](types)
        case '[type a <: HChain; HSingleton[Option[a]]] => buildTidyType[a] match {
          case '[b] => go[Option[b] *: Acc](types)
        }
        case '[type a <: HChain; type b <: HChain; HSingleton[Either[a, b]]] => buildTidyType[a] match {
          case '[c] => buildTidyType[b] match {
            case '[d] => go[Either[c, d] *: Acc](types)
          }
        }
        case '[type a <: HChain; type b <: HChain; HSingleton[Ior[a, b]]] => buildTidyType[a] match {
          case '[c] => buildTidyType[b] match {
            case '[d] => go[Ior[c, d] *: Acc](types)
          }
        }
        case '[HSingleton[a]] => go[a *: Acc](types)
        case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => go[Acc](Type.of[b] :: Type.of[a] :: types)
      }
    }

    val tpe = go[EmptyTuple](Type.of[A] :: Nil)
    tpe match {
      case '[EmptyTuple] => Type.of[Unit]
      case '[Tuple1[a]]  => Type.of[a]
      case _             => tpe
    }
  }

  transparent inline def tidyHChain[A <: HChain](xs: A) = ${ tidyHChainCode('xs) }

  private def tidyHChainCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    val tpe = buildTidyType[A]
    tpe match {
      case '[a] => '{ Tuple.fromArray($xs.toList.toArray).asInstanceOf[a] }
    }
  }
}
