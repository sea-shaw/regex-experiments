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
    |    override protected def tconsBuildFunction[t0: Type](tail: Types)(using Quotes): BuildFunction[LCons[t0, tail.ToLeaves], ?] = {
    |      type ToLeaves = LCons[t0, tail.ToLeaves]
    |      tail match {
    |        case TNil => new BuildFunction[ToLeaves, t0] {
    |          override def apply(leaves: ToLeaves)(using Quotes): Expr[t0] = leaves.head
    |        }
    |        case TCons(given Type[t1], tail1) => tail1 match {
    |          case TNil => new BuildFunction[ToLeaves, (t1, t0)] {
    |            override def apply(leaves: ToLeaves)(using Quotes): Expr[(t1, t0)] = {
    |              val LCons(e0, LCons(e1, LNil)) = leaves.asInstanceOf[LCons[t0, LCons[t1, LNil]]]
    |              '{ ($e1, $e0) }
    |            }
    |          }
    |          case TCons(given Type[t2], tail2) => tail2 match {
    |            case TNil => new BuildFunction[ToLeaves, (t2, t1, t0)] {
    |              override def apply(leaves: ToLeaves)(using Quotes): Expr[(t2, t1, t0)] = {
    |                val LCons(e0, LCons(e1, LCons(e2, LNil))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LNil]]]]
    |                '{ ($e2, $e1, $e0) }
    |              }
    |            }
    |            case TCons(given Type[t3], tail3) => tail3 match {
    |              case TNil => new BuildFunction[ToLeaves, (t3, t2, t1, t0)] {
    |                override def apply(leaves: ToLeaves)(using Quotes): Expr[(t3, t2, t1, t0)] = {
    |                  val LCons(e0, LCons(e1, LCons(e2, LCons(e3, LNil)))) = leaves.asInstanceOf[LCons[t0, LCons[t1, LCons[t2, LCons[t3, LNil]]]]]
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

    def tupleCase(n: Int): String = {
      if (n <= 22) {
        val indices = (0 until n)
        val tupleType = indices.reverse.map(i => s"t$i").mkString("(", ", ", ")")
        val tupleExpr = indices.reverse.map(i => s"$$e$i").mkString("'{ (", ", ", ") }")
        val leavesPattern = indices.map(i => s"LCons(e$i, ").mkString("", "", s"LNil${")" * n}")
        val leavesType = indices.map(i => s"LCons[t$i, ").mkString("", "", s"LNil${"]" * n}")
        s"""|case TCons(given Type[t$n], tail$n) => tail$n match {
            |  case TNil => new BuildFunction[ToLeaves, $tupleType] {
            |    override def apply(leaves: ToLeaves)(using Quotes): Expr[$tupleType] = {
            |      val $leavesPattern = leaves.asInstanceOf[$leavesType]
            |      $tupleExpr
            |    }
            |  }
            |${tupleCase(n + 1).indent(2)}
            |}
        """.stripMargin
      } else {
        "case _ => ???"
      }
    }
}
