package experiments.macros

import experiments.macros.hlist.{Concat, HCons, HList, HNil}
import scala.quoted.{Expr, Quotes, Type}

object ast {
  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[A]): (Expr[(Option[A], Boolean)], Int)
  }

  case class Capture[A <: HList: Type](r: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[String, A]]): (Expr[(Option[HCons[String, A]], Boolean)], Int) = ???
  }

  case class Alt[A <: HList: Type, B <: HList: Type](left: Regex[A], right: Regex[B]) extends Regex[HCons[Either[A, B], HNil]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[Either[A, B], HNil]]): (Expr[(Option[HCons[Either[A, B], HNil.type]], Boolean)], Int) = ???
  }

  case class Opt[A <: HList: Type](r: Regex[A]) extends Regex[HCons[Option[A], HNil]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[Option[A], HNil]]): (Expr[(Option[HCons[Option[A], HNil.type]], Boolean)], Int) = {
      val (sanitisedValue, j) = r.sanitiseCode(groups, i)
      val sanitised = '{
        val (caps, any) = $sanitisedValue
        (Some(HCons(caps, HNil)), any)
      }
      (sanitised, j)
    }
  }
  
  case class Cat[A <: HList: Type, B <: HList: Type](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[Concat[A, B]]): (Expr[(Option[Concat[A, B]], Boolean)], Int) = ???
  }

  case class Lit(s: String) extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HNil]): (Expr[(Option[HNil], Boolean)], Int) = {
      val sanitised = '{ (Some(HNil), false) }
      (sanitised, i)
    }
  }
}
