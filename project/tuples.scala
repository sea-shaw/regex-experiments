import sbt.*

object tuples {

  def gen(dir: File): Seq[File] = {
    val file = filename(dir)
    IO.write(file, content)
    Seq(file)
  }

  def filename(root: File): File = root / "scala"/ "macros" / "tidy.scala"

  val content = """|package experiments.macros
    |
    |import experiments.macros.ast.AST
    |import scala.quoted.{Expr, Quotes, Type}
    |
    |object tidy {
    |  trait Tidy extends AST {
    |    override protected def tconsBuildFunction[T0: Type](tail: Types)(using Quotes): BuildFunction[LCons[T0, tail.ToLeaves], ?] = {
    |      type ToLeaves = LCons[T0, tail.ToLeaves]
    |      tail match {
    |        case TNil => new BuildFunction[ToLeaves, T0] {
    |          override def apply(leaves: ToLeaves)(using Quotes): Expr[T0] = leaves.head
    |        }
    |        case TCons(given Type[t1], tail1) => tail1 match {
    |          case TNil => new BuildFunction[ToLeaves, (t1, T0)] {
    |            override def apply(leaves: ToLeaves)(using Quotes): Expr[(t1, T0)] = {
    |              val LCons(e0, LCons(e1, LNil)) = leaves.asInstanceOf[LCons[T0, LCons[t1, LNil]]]
    |              '{ ($e1, $e0) }
    |            }
    |          }
    |          case TCons(given Type[t2], tail2) => tail2 match {
    |            case TNil => new BuildFunction[ToLeaves, (t2, t1, T0)] {
    |              override def apply(leaves: ToLeaves)(using Quotes): Expr[(t2, t1, T0)] = {
    |                val LCons(e0, LCons(e1, LCons(e2, LNil))) = leaves.asInstanceOf[LCons[T0, LCons[t1, LCons[t2, LNil]]]]
    |                '{ ($e2, $e1, $e0) }
    |              }
    |            }
    |            case TCons(given Type[t3], tail3) => tail3 match {
    |              case TNil => new BuildFunction[ToLeaves, (t3, t2, t1, T0)] {
    |                override def apply(leaves: ToLeaves)(using Quotes): Expr[(t3, t2, t1, T0)] = {
    |                  val LCons(e0, LCons(e1, LCons(e2, LCons(e3, LNil)))) = leaves.asInstanceOf[LCons[T0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]]
    |                  '{ ($e3, $e2, $e1, $e0) }
    |                }
    |              }
    |              case TCons(_, _) => ???
    |            }
    |          }
    |        }
    |      }
    |    }
    |  }
    |}
    |""".stripMargin
}
