package experiments.macros

import experiments.macros.hcollections.hchain.HChain
import scala.quoted.{Expr, Quotes, Type}

object tidy {

  type Rep = Boolean

  trait Node[F[_ <: Rep] <: HChain] {
    def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[F[R], nodes.ToChains], types.ToLeaves, ?]

    final def tidyFunction[R <: Rep: Type](using Quotes): TidyFunction[F[R], ?] = {
      flattenFunction[R](NNil, TNil) match {
        case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
          override def apply(xs: Expr[F[R]])(using Quotes): Expr[a] = {
            flatten(CCons(xs, CNil), LNil)
          }
        }
      }
    }
  }

  sealed trait Nodes {
    type ToChains <: Chains

    def flattenFunction(types: Types)(using Quotes): FlattenFunction[ToChains, types.ToLeaves, ?]
  }

  case object NNil extends Nodes {
    type ToChains = CNil

    override def flattenFunction(types: Types)(using Quotes): FlattenFunction[CNil, types.ToLeaves, ?] = {
      types.buildFunction match {
        case build @ BuildFunction(given Type[a]) => new FlattenFunction[CNil, types.ToLeaves, a] {
          override def apply(chains: CNil, leaves: types.ToLeaves)(using Quotes): Expr[a] = {
            build(leaves)
          }
        }
      }
    }
  }

  case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, N <: Nodes & Singleton](head: Node[F], tail: N) extends Nodes {
    type ToChains = CCons[F[R], tail.ToChains]

    override def flattenFunction(types: Types)(using Quotes): FlattenFunction[CCons[F[R], tail.ToChains], types.ToLeaves, ?] = {
      head.flattenFunction[R](tail, types)
    }
  }

  sealed trait Types {
    type ToLeaves <: Leaves
    def buildFunction(using Quotes): BuildFunction[ToLeaves, ?]
  }

  case object TNil extends Types {
    type ToLeaves = LNil

    override def buildFunction(using Quotes): BuildFunction[LNil, ?] = new BuildFunction[LNil, Unit] {
      override def apply(leaves: LNil)(using Quotes): Expr[Unit] = '{ () }
    }
  }

  case class TCons[T0, T <: Types & Singleton](head: Type[T0], tail: T) extends Types {
    type ToLeaves = LCons[T0, tail.ToLeaves]
    override def buildFunction(using Quotes): BuildFunction[ToLeaves, ?] = {
      given Type[T0] = head

      tail match {
        case TNil => new BuildFunction[ToLeaves, T0] {
          override def apply(leaves: ToLeaves)(using Quotes): Expr[T0] = leaves.head
        }
        case TCons(given Type[t1], tail1) => {
          tail1 match {
            case TNil => new BuildFunction[ToLeaves, (t1, T0)] {
              override def apply(leaves: ToLeaves)(using Quotes): Expr[(t1, T0)] = {
                val LCons(e0, LCons(e1, LNil)) = leaves.asInstanceOf[LCons[T0, LCons[t1, LNil]]]
                '{ ($e1, $e0) }
              }
            }
            case TCons(given Type[t2], tail2) => {
              tail2 match {
                case TNil => new BuildFunction[ToLeaves, (t2, t1, T0)] {
                  override def apply(leaves: ToLeaves)(using Quotes): Expr[(t2, t1, T0)] = {
                    val LCons(e0, LCons(e1, LCons(e2, LNil))) = leaves.asInstanceOf[LCons[T0, LCons[t1, LCons[t2, LNil]]]]
                    '{ ($e2, $e1, $e0) }
                  }
                }
                case TCons(given Type[t3], tail3) => {
                  tail3 match {
                    case TNil => new BuildFunction[ToLeaves, (t3, t2, t1, T0)] {
                      override def apply(leaves: ToLeaves)(using Quotes): Expr[(t3, t2, t1, T0)] = {
                        val LCons(e0, LCons(e1, LCons(e2, LCons(e3, LNil)))) = leaves.asInstanceOf[LCons[T0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]]
                        '{ ($e3, $e2, $e1, $e0) }
                      }
                    }
                    case TCons(_, _) => ???
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  sealed trait Chains
  type CNil = CNil.type
  case object CNil extends Chains
  case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

  sealed trait Leaves
  type LNil = LNil.type
  case object LNil extends Leaves
  case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

  abstract class TidyFunction[A <: HChain, B](using val tpe: Type[B]) {
    def apply(xs: Expr[A])(using Quotes): Expr[B]
  }

  object TidyFunction {
    def unapply[A <: HChain, B](tidyFunction: TidyFunction[A, B]): Tuple1[Type[B]] = Tuple1(tidyFunction.tpe)
  }

  abstract class FlattenFunction[C <: Chains, L <: Leaves, A](using val tpe: Type[A]) {
    def apply(chains: C, leaves: L)(using Quotes): Expr[A]
  }

  object FlattenFunction {
    def unapply[C <: Chains, L <: Leaves, A](tidyFunction: FlattenFunction[C, L, A]): Tuple1[Type[A]] = Tuple1(tidyFunction.tpe)
  }

  abstract class BuildFunction[L <: Leaves, A](using val tpe: Type[A]) {
    def apply(leaves: L)(using Quotes): Expr[A]
  }

  object BuildFunction {
    def unapply[L <: Leaves, A](buildFunction: BuildFunction[L, A]): Tuple1[Type[A]] = Tuple1(buildFunction.tpe)
  }
}
