package experiments.macros

import cats.data.Ior
import cats.syntax.all.*
import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty, HNonEmpty}
import scala.annotation.unused
import scala.quoted.{Expr, Quotes, Type}

object tidy {

  private sealed trait Nodes
  private type NNil = NNil.type
  private case object NNil extends Nodes
  private case class NCons[+A <: HNonEmpty, +N <: Nodes](head: Expr[A], tail: N) extends Nodes

  private sealed trait Leaves
  private type LNil = LNil.type
  private case object LNil extends Leaves
  private case class LCons[+A, +L <: Leaves](head: Expr[A], tail: L) extends Leaves

  sealed abstract class TidyFunction[-A <: HChain, B](val tpe: Type[B]) {
    def apply(xs: Expr[A])(using Quotes): Expr[B]
  }

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    tidyFunction[A](xs)
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {

    sealed abstract class FlattenFunction[-N <: Nodes, -L <: Leaves, B](val tpe: Type[B]) {
      def apply(nodes: N, leaves: L)(using Quotes): Expr[B]
    }
    sealed abstract class BuildFunction[-L <: Leaves, B: Type] {
      final val tpe: Type[B] = summon[Type[B]]
      def apply(leaves: L)(using Quotes): Expr[B] 
    }

    def flattenFunction[N <: Nodes: Type, L <: Leaves: Type](using Quotes): FlattenFunction[N, L, ?] = Type.of[N] match {
      case '[NNil] => {
        val build = buildFunction[L]
        new FlattenFunction[N, L, build.tpe.Underlying](build.tpe) {
          override def apply(nodes: N, leaves: L)(using Quotes) = {
            build(leaves)
          }
        }
      }
      case '[NCons[n, ns]] => Type.of[n] match {
        case '[type b <: HChain; HSingleton[Option[b]]] => {
          val inner: TidyFunction[b, ?] = tidyFunction[b]
          type B = inner.tpe.Underlying
          given Type[B] = inner.tpe

          val flatten = flattenFunction[ns, LCons[Option[B], L]]
          new FlattenFunction[N, L, flatten.tpe.Underlying](flatten.tpe) {
            override def apply(nodes: N, leaves: L)(using Quotes) = {
              // TODO
              val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Option[b]], ns]]
              flatten(tail, LCons('{ $node.value.map(x => ${ inner('x) }) }, leaves))
            }
          }
        }
        case '[type b <: HChain; type c <: HChain; HSingleton[Either[b, c]]] => {
          val left: TidyFunction[b, ?] = tidyFunction[b]
          type Left = left.tpe.Underlying
          given Type[Left] = left.tpe

          val right: TidyFunction[c, ?] = tidyFunction[c]
          type Right = right.tpe.Underlying
          given Type[Right] = right.tpe

          val flatten = flattenFunction[ns, LCons[Either[Left, Right], L]]

          new FlattenFunction[N, L, flatten.tpe.Underlying](flatten.tpe) {
            override def apply(nodes: N, leaves: L)(using Quotes) = {
              // TODO
              val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Either[b, c]], ns]]
              flatten(tail, LCons('{ $node.value.bimap(x => ${ left('x) }, x => ${ right('x) }) }, leaves))
            }
          }
        }
        case '[type b <: HChain; type c <: HChain; HSingleton[Ior[b, c]]] => {
          val left: TidyFunction[b, ?] = tidyFunction[b]
          type B = left.tpe.Underlying
          given Type[B] = left.tpe

          val right: TidyFunction[c, ?] = tidyFunction[c]
          type C = right.tpe.Underlying
          given Type[C] = right.tpe

          val flatten = flattenFunction[ns, LCons[Ior[B, C], L]]

          new FlattenFunction[N, L, flatten.tpe.Underlying](flatten.tpe) {
            override def apply(nodes: N, leaves: L)(using Quotes) = {
              // TODO
              val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Ior[b, c]], ns]]
              flatten(tail, LCons('{ $node.value.bimap(x => ${ left('x) }, x => ${ right('x) }) }, leaves))
            }
          }
        }
        case '[HSingleton[a]] => {
          val flatten = flattenFunction[ns, LCons[a, L]]
          new FlattenFunction[N, L, flatten.tpe.Underlying](flatten.tpe) {
            override def apply(nodes: N, leaves: L)(using Quotes) = {
              // TODO: Ewwwwww
              val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[a], ns]]
              flatten(tail, LCons('{ $node.value }, leaves))
            }
          }
        }
        case '[type a <: HNonEmpty; type b <: HNonEmpty; HAppend[a, b]] => {
          val flatten: FlattenFunction[NCons[b, NCons[a, ns]], L, ?] = flattenFunction[NCons[b, NCons[a, ns]], L]
          type B = flatten.tpe.Underlying
          given Type[B] = flatten.tpe
          new FlattenFunction[N, L, B](flatten.tpe) {
            override def apply(nodes: N, leaves: L)(using Quotes): Expr[B] = {
              // TODO: Ewwww
              val NCons(node, tail) = nodes.asInstanceOf[NCons[HAppend[a, b], ns]]
              '{
                val left = $node.left
                val right = $node.right
                ${ flatten(NCons('{ right }, NCons('{left}, tail)), leaves) }
              }
            }
          }
        }
      }
    }

    def buildFunction[L <: Leaves: Type](using Quotes): BuildFunction[L, ?] = Type.of[L] match {
      case '[LNil] => {
        new BuildFunction[L, Unit] {
          override def apply(@unused leaves: L)(using Quotes): Expr[Unit] = '{ () }
        }
      }
      case '[LCons[t0, LNil]] => {
        new BuildFunction[L, t0] {
          override def apply(leaves: L)(using Quotes): Expr[t0] = {
            // TODO: Ewww
            val LCons(e0, LNil) = leaves.asInstanceOf[LCons[t0, LNil]]
            e0
          }
        }
      }
      case '[LCons[t0, LCons[t1, LNil]]] => {
        new BuildFunction[L, (t0, t1)] {
          override def apply(leaves: L)(using Quotes) = {
            // TODO: Ewww
            val LCons(e0, LCons(e1, LNil)) = leaves.asInstanceOf[LCons[t0, LCons[t1, LNil]]]
            '{ ($e0, $e1) }
          }
        }
      }
      case '[LCons[t0, LCons[t1, LCons[t2, LNil]]]] => {
        new BuildFunction[L, (t0, t1, t2)] {
          override def apply(leaves: L)(using Quotes) = {
            // TODO: Ewww
            val LCons(e0, LCons(e1, LCons(e2, LNil))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LNil]]]]
            '{ ($e0, $e1, $e2) }
          }
        }
      }
      case '[LCons[t0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]] => {
        new BuildFunction[L, (t0, t1, t2, t3)] {
          override def apply(leaves: L)(using Quotes) = {
            // TODO: Ewww
            val LCons(e0, LCons(e1, LCons(e2, LCons(e3, LNil)))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]]
            '{ ($e0, $e1, $e2, $e3) }
          }
        }
      }
    }

    Type.of[A] match {
      case '[HEmpty] => new TidyFunction[A, Unit](Type.of[Unit]) {
        override def apply(xs: Expr[A])(using Quotes): Expr[Unit] = '{ () }
      }
      case '[type a <: HNonEmpty; a] => {
        val flatten = flattenFunction[NCons[a, NNil], LNil]
        new TidyFunction[A, flatten.tpe.Underlying](flatten.tpe) {
          override def apply(xs: Expr[A])(using Quotes) = {
            flatten(NCons(xs.asExprOf[a], NNil), LNil)
          }
        }
      }
    }
  }
}
