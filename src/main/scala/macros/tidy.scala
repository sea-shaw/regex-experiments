package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, HEmpty, HNonEmpty}
import scala.annotation.unused
import scala.quoted.{Expr, Quotes, Type, quotes}

object tidy {

  private sealed trait Nodes
  private type NNil = NNil.type
  private case object NNil extends Nodes
  private case class NCons[A <: HNonEmpty, N <: Nodes](head: Expr[A], tail: N) extends Nodes

  private sealed trait Leaves
  private type LNil = LNil.type
  private case object LNil extends Leaves
  private case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

  class TidyFunction[A <: HChain, B](val tidy: Expr[A] => Quotes ?=> Expr[B], val tpe: Type[B])

  def tidyFunction2[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {

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

    // val tidyHList: Tidy[NCons[A, NNil], LNil, ?] = go[NCons[A, NNil], LNil]
    // type B = tidyHList.tpe.Underlying
    // TidyFunction[A, B](xs => tidyHList.tidy(NCons(xs, NNil), LNil), tidyHList.tpe)
    ???
  }

  def tidyFunction[A <: HChain: Type](using Quotes): TidyFunction[A, ?] = {
    val typed: Typed[?] = tidyType[A]
    type B = typed.tpe.Underlying
    given Type[B] = typed.tpe

    def tidy(xs: Expr[A])(using Quotes): Expr[B] = tidyCode[A](xs).asExprOf[B]

    TidyFunction(tidy, typed.tpe)
  }

  transparent inline def tidy[A <: HChain](xs: A) = ${ tidyCode('xs) }

  def tidyCode[A <: HChain: Type](xs: Expr[A])(using Quotes): Expr[?] = {

    case class TypedExpr[A](expr: Expr[A], tpe: Type[A])

    def go(nodes: List[TypedExpr[? <: HChain]], leaves: List[TypedExpr[?]])(using Quotes): Expr[?] = nodes match {
      case Nil     => build(leaves)
      case TypedExpr(node, tpe) :: nodes => tpe match {
        case '[HEmpty] => go(nodes, leaves)
        case '[HSingleton[a]] => {
          val singleton = node.asExprOf[HSingleton[a]]
          go(nodes, TypedExpr('{ $singleton.value }, Type.of[a]) :: leaves)
        }
        case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
          val append = node.asExprOf[HAppend[a, b]]
          '{
            val left = $append.left
            val right = $append.right
            ${ go(TypedExpr('{ right }, Type.of[b]) :: TypedExpr('{ left }, Type.of[a]) :: nodes, leaves) }
          }
        }
      }
    }

    def build(leaves: List[TypedExpr[?]])(using Quotes): Expr[?] = {
      import quotes.reflect.{Position, report}
      val elems = leaves.toVector

      elems match {
        case Vector() => '{ () }
        case Vector(TypedExpr(e0, _)) => e0
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying

          given Type[T0] = t0
          given Type[T1] = t1

          '{ ($e0, $e1) }
        }
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1), TypedExpr(e2, t2)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying

          given Type[T0] = t0
          given Type[T1] = t1
          given Type[T2] = t2

          '{ ($e0, $e1, $e2) }
        }
        case Vector(TypedExpr(e0, t0), TypedExpr(e1, t1), TypedExpr(e2, t2), TypedExpr(e3, t3)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying
          type T3 = t3.Underlying

          given Type[T0] = t0
          given Type[T1] = t1
          given Type[T2] = t2
          given Type[T3] = t3

          '{ ($e0, $e1, $e2, $e3) }
        }
        case _ => report.errorAndAbort(s"Unsupported tuple size ${elems.size}", Position.ofMacroExpansion)
      }
    }

    go(TypedExpr(xs, Type.of[A]) :: Nil, Nil)
  }

  case class Typed[A: Type](tpe: Type[A])
  object Typed {
    def of[A: Type](using Quotes) = Typed(Type.of[A])
  }

  def tidyType[A <: HChain: Type](using Quotes): Typed[?] = {

    def go(nodes: List[Typed[? <: HChain]], leaves: List[Typed[?]])(using Quotes): Typed[?] = nodes match {
      case Nil => build(leaves)
      case node :: nodes => node.tpe match {
        case '[HEmpty] => go(nodes, leaves)
        case '[HSingleton[a]] => go(nodes, Typed.of[a] :: leaves)
        case '[type a <: HChain; type b <: HChain; HAppend[a, b]] => {
          go(Typed.of[b] :: Typed.of[a] :: nodes, leaves)
        }
      }
    }

    def build(leaves: List[Typed[?]])(using Quotes): Typed[?] = {
      import quotes.reflect.{Position, report}

      val types = leaves.toVector
      types match {
        case Vector() => Typed.of[Unit]
        case Vector(t0) => t0
        case Vector(Typed(t0), Typed(t1)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          Typed.of[(T0, T1)]
        }
        case Vector(Typed(t0), Typed(t1), Typed(t2)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying
          Typed.of[(T0, T1, T2)]
        }
        case Vector(Typed(t0), Typed(t1), Typed(t2), Typed(t3)) => {
          type T0 = t0.Underlying
          type T1 = t1.Underlying
          type T2 = t2.Underlying
          type T3 = t3.Underlying
          Typed.of[(T0, T1, T2, T3)]
        }
        case _ => report.errorAndAbort(s"Unsupported tuple size ${types.size}", Position.ofMacroExpansion)
      }
    }

    go(Typed(Type.of[A]) :: Nil, Nil)
  }
}
