package experiments.macros

import cats.collections.Diet
import cats.data.Chain
import scala.quoted.{Expr, Quotes, Type}
import parsley.templates.{PureParserBridge0, PureParserBridge1, PureParserBridge2}
import cats.data.NonEmptyList

object ast2 {
  type Groups = Array[Option[String]]

  sealed abstract class Elem[A](using val tpe: Type[A]) {
    def any(groups: Expr[Groups])(using Quotes): Expr[Boolean]
    def apply(groups: Expr[Groups])(using Quotes): Expr[A]
  }
  object Elem {
    def unapply[A](elemFunction: Elem[A]): Tuple1[Type[A]] = Tuple1(elemFunction.tpe)
  }

  sealed abstract class Regex {
    val numCaptures: Int
    def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]]
    final def tidyFunction(i: Int)(using Quotes): Elem[?] = elemFunctions(i).toList match {
      case Nil => new Elem[Unit] {
        override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = '{ false }
        override def apply(groups: Expr[Groups])(using Quotes): Expr[Unit] = '{ () }
      }
      case (elem0 @ Elem(given Type[t0])) :: tail0 => tail0 match {
        case Nil => elem0
        case (elem1 @ Elem(given Type[t1])) :: tail1 => tail1 match {
          case Nil => new Elem[Tuple2[t0, t1]] {
            override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
              '{ ${ elem0.any(groups) } || ${ elem1.any(groups) } }
            }

            override def apply(groups: Expr[Groups])(using Quotes): Expr[Tuple2[t0, t1]] = {
              '{ Tuple2(${ elem0(groups) }, ${ elem1(groups) }) }
            }
          }
          case (elem2 @ Elem(given Type[t2])) :: tail1 => tail1 match {
            case Nil => new Elem[Tuple3[t0, t1, t2]] {
              override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
                '{ ${ elem0.any(groups) } || ${ elem1.any(groups) } || ${ elem2.any(groups) } }
              }

              override def apply(groups: Expr[Groups])(using Quotes): Expr[Tuple3[t0, t1, t2]] = {
                '{ Tuple3(${ elem0(groups) }, ${ elem1(groups) }, ${ elem2(groups) }) }
              }
            }
            case (elem3 @ Elem(given Type[t3])) :: tail1 => tail1 match {
              case Nil => new Elem[Tuple4[t0, t1, t2, t3]] {
                override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
                  '{ ${ elem0.any(groups) } || ${ elem1.any(groups) } || ${ elem2.any(groups) } }
                }

                override def apply(groups: Expr[Groups])(using Quotes): Expr[Tuple4[t0, t1, t2, t3]] = {
                  '{ Tuple4(${ elem0(groups) }, ${ elem1(groups) }, ${ elem2(groups) }, ${ elem3(groups) }) }
                }
              }
              case _   => ???
            }
          }
        }
      }
    }
  }

  sealed abstract class Empty extends Regex {
    override final def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]] = Chain.nil
  }

  sealed abstract class EmptyLeaf extends Empty {
    override final val numCaptures: Int = 0
  }

  type Dot = Dot.type
  case object Dot extends EmptyLeaf with PureParserBridge0[Dot]

  case class Lit(c: Int) extends EmptyLeaf
  object Lit extends PureParserBridge1[Int, Lit]

  case class Class(cs: Diet[Int]) extends EmptyLeaf
  object Class extends PureParserBridge1[Diet[Int], Class]

  case class Capture(inner: Regex) extends Regex {
    override val numCaptures: Int = 1 + inner.numCaptures

    override def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]] = {
      val head = new Elem[String] {
        override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
          val idx = Expr(i)
          '{ $groups($idx).isDefined }
        }
        override def apply(groups: Expr[Groups])(using Quotes): Expr[String] = {
          val idx = Expr(i)
          '{ $groups($idx).get }
        }
      }
      val tail = inner.elemFunctions(i + 1)
      head +: tail
    }
  }
  object Capture extends PureParserBridge1[Regex, Capture]

  case class Cat(left: Regex, right: Regex) extends Regex {
    override val numCaptures: Int = left.numCaptures + right.numCaptures

    override def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]] = {
      left.elemFunctions(i) ++ right.elemFunctions(i + left.numCaptures)
    }
  }
  object Cat extends PureParserBridge1[NonEmptyList[Regex], Regex] {
    override def apply(regexes: NonEmptyList[Regex]): Regex = {
      regexes.tail.foldLeft(regexes.head)(Cat(_, _))
    }
  }

  case class Opt(inner: Regex) extends Regex {
    override val numCaptures: Int = inner.numCaptures

    override def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]] = {
      if (numCaptures == 0) {
        Chain.nil
      } else {
        inner.tidyFunction(i) match {
          case elem @ Elem(given Type[a]) => {
            val optElem = new Elem[Option[a]] {
              override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
                elem.any(groups)
              }

              override def apply(groups: Expr[Groups])(using Quotes): Expr[Option[a]] = {
                '{
                  if (${ elem.any(groups) }) {
                    Some(${ elem(groups) })
                  } else {
                    None
                  }
                }
              }
            }
            Chain.one(optElem)
          }
        }
      }
    }
  }
  object Opt extends PureParserBridge1[Regex, Opt]

  case class Alt(left: Regex, right: Regex) extends Regex {
    override val numCaptures: Int = left.numCaptures + right.numCaptures

    override def elemFunctions(i: Int)(using Quotes): Chain[Elem[?]] = {
      if (numCaptures == 0) {
        Chain.nil
      } else {
        (left.tidyFunction(i), right.tidyFunction(i + left.numCaptures)) match {
          case (leftElem @ Elem(given Type[a]), rightElem @ Elem(given Type[b])) => {
            val altElem = new Elem[Either[a, b]] {
              override def any(groups: Expr[Groups])(using Quotes): Expr[Boolean] = {
                '{ ${ leftElem.any(groups) } || ${ rightElem.any(groups) } }
              }

              override def apply(groups: Expr[Groups])(using Quotes): Expr[Either[a, b]] = {
                '{
                  if (${ leftElem.any(groups) }) {
                    Left(${ leftElem(groups) })
                  } else {
                    Right(${ rightElem(groups) })
                  }
                }
              }
            }
            Chain.one(altElem)
          }
        }
      }
    }
  }
  object Alt extends PureParserBridge2[Regex, Regex, Alt]
}
