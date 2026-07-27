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
  private case class NCons[A <: HNonEmpty, N <: Nodes](head: Expr[A], tail: N) extends Nodes

  private sealed trait Leaves
  private type LNil = LNil.type
  private case object LNil extends Leaves
  private case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

  case class TidyFunction[A <: HChain, B](tidy: Expr[A] => Quotes ?=> Expr[B], tpe: Type[B])

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  private def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {
    val tidy = tidyFunction[A]
    tidy.tidy(xs)
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {

    case class Tidy[-N <: Nodes, -L <: Leaves, B](tidy: (N, L) => Quotes ?=> Expr[B], tpe: Type[B])
    case class Build[-L <: Leaves, B](build: L => Quotes ?=> Expr[B], tpe: Type[B])

    def go[N <: Nodes: Type, L <: Leaves: Type](using Quotes): Tidy[N, L, ?] = Type.of[N] match {
      case '[NNil] => {
        val build = buildFunction[L]
        type B = build.tpe.Underlying
        def tidy(@unused nodes: N, leaves: L)(using Quotes): Expr[B] = {
          build.build(leaves)
        }
        Tidy(tidy, build.tpe)
      }
      case '[NCons[n, ns]] => Type.of[n] match {
        case '[type b <: HChain; HSingleton[Option[b]]] => {
          val inner: TidyFunction[b, ?] = tidyFunction[b]
          type B = inner.tpe.Underlying
          given Type[B] = inner.tpe

          val next = go[ns, LCons[Option[B], L]]
          type C = next.tpe.Underlying

          def tidy(nodes: N, leaves: L)(using Quotes): Expr[C] = {
            // TODO
            val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Option[b]], ns]]
            next.tidy(tail, LCons('{ $node.value.map(x => ${ inner.tidy('x) }) }, leaves))
          }

          Tidy(tidy, next.tpe)
        }
        case '[type b <: HChain; type c <: HChain; HSingleton[Either[b, c]]] => {
          val left: TidyFunction[b, ?] = tidyFunction[b]
          type B = left.tpe.Underlying
          given Type[B] = left.tpe

          val right: TidyFunction[c, ?] = tidyFunction[c]
          type C = right.tpe.Underlying
          given Type[C] = right.tpe

          val next = go[ns, LCons[Either[B, C], L]]
          type D = next.tpe.Underlying

          def tidy(nodes: N, leaves: L)(using Quotes): Expr[D] = {
            // TODO
            val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Either[b, c]], ns]]
            next.tidy(tail, LCons('{ $node.value.bimap(x => ${ left.tidy('x) }, x => ${ right.tidy('x) }) }, leaves))
          }

          Tidy(tidy, next.tpe)
        }
        case '[type b <: HChain; type c <: HChain; HSingleton[Ior[b, c]]] => {
          val left: TidyFunction[b, ?] = tidyFunction[b]
          type B = left.tpe.Underlying
          given Type[B] = left.tpe

          val right: TidyFunction[c, ?] = tidyFunction[c]
          type C = right.tpe.Underlying
          given Type[C] = right.tpe

          val next = go[ns, LCons[Ior[B, C], L]]
          type D = next.tpe.Underlying

          def tidy(nodes: N, leaves: L)(using Quotes): Expr[D] = {
            // TODO
            val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[Ior[b, c]], ns]]
            next.tidy(tail, LCons('{ $node.value.bimap(x => ${ left.tidy('x) }, x => ${ right.tidy('x) }) }, leaves))
          }

          Tidy(tidy, next.tpe)
        }
        case '[HSingleton[a]] => {
          val next = go[ns, LCons[a, L]]
          type B = next.tpe.Underlying
          def tidy(nodes: N, leaves: L)(using Quotes): Expr[B] = {
            // TODO: Ewwwwww
            val NCons(node, tail) = nodes.asInstanceOf[NCons[HSingleton[a], ns]]
            next.tidy(tail, LCons('{ $node.value }, leaves))
          }
          Tidy(tidy, next.tpe)
        }
        case '[type a <: HNonEmpty; type b <: HNonEmpty; HAppend[a, b]] => {
          val next: Tidy[NCons[b, NCons[a, ns]], L, ?] = go[NCons[b, NCons[a, ns]], L]
          type B = next.tpe.Underlying
          given Type[B] = next.tpe
          def tidy(nodes: N, leaves: L)(using Quotes): Expr[B] = {
            // TODO: Ewwww
            val NCons(node, tail) = nodes.asInstanceOf[NCons[HAppend[a, b], ns]]
            '{
              val left = $node.left
              val right = $node.right
              ${ next.tidy(NCons('{ right }, NCons('{left}, tail)), leaves) }
            }
          }
          Tidy(tidy, next.tpe)
        }
      }
    }

    def buildFunction[L <: Leaves: Type](using Quotes): Build[L, ?] = Type.of[L] match {
      case '[LNil] => Build(_ => _ ?=> '{ () }, Type.of[Unit])
      case '[LCons[t0, LNil]] => {
        def build(leaves: L) = {
          // TODO: Ewww
          val LCons(e0, LNil) = leaves.asInstanceOf[LCons[t0, LNil]]
          e0
        }
        Build(build, Type.of[t0])
      }
      case '[LCons[t0, LCons[t1, LNil]]] => {
        def build(leaves: L)(using Quotes) = {
          // TODO: Ewww
          val LCons(e0, LCons(e1, LNil)) = leaves.asInstanceOf[LCons[t0, LCons[t1, LNil]]]
          '{ ($e0, $e1) }
        }
        Build(build, Type.of[(t0, t1)])
      }
      case '[LCons[t0, LCons[t1, LCons[t2, LNil]]]] => {
        def build(leaves: L)(using Quotes) = {
          // TODO: Ewww
          val LCons(e0, LCons(e1, LCons(e2, LNil))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LNil]]]]
          '{ ($e0, $e1, $e2) }
        }
        Build(build, Type.of[(t0, t1, t2)])
      }
      case '[LCons[t0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]] => {
        def build(leaves: L)(using Quotes) = {
          // TODO: Ewww
          val LCons(e0, LCons(e1, LCons(e2, LCons(e3, LNil)))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]]
          '{ ($e0, $e1, $e2, $e3) }
        }
        Build(build, Type.of[(t0, t1, t2, t3)])
      }
    }

    Type.of[A] match {
      case '[HEmpty] => TidyFunction[A, Unit](_ => _ ?=> '{ () }, Type.of[Unit])
      case '[type a <: HNonEmpty; a] => {
        val tidyFunction = go[NCons[a, NNil], LNil]
        TidyFunction(xs => _ ?=> tidyFunction.tidy(NCons(xs.asExprOf[a], NNil), LNil), tidyFunction.tpe)
      }
    }
  }
}
