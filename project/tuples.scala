import sbt.*

object tuples {

  private val minSize = 2
  private val maxSize = 22
  private val indentSize = 2

  private val content = s"""|package experiments.macros
    |
    |import experiments.macros.ast.AST
    |import scala.quoted.{Expr, Quotes, Type, quotes}
    |
    |object tidy {
    |  trait Tidy extends AST {
    |    override protected def buildFunction[L <: Leaves](types: Types[L])(using Quotes): BuildFunction[L, ?] = {
    |      types match {
    |        case TNil => new BuildFunction[LNil, Unit] {
    |          override def apply(leaves: LNil)(using Quotes): Expr[Unit] = {
    |            '{ () }
    |          }
    |        }
    |        case TCons(given Type[t0], tail0) => tail0 match {
    |          case TNil => new BuildFunction[LCons[t0, LNil], t0] {
    |            override def apply(leaves: LCons[t0, LNil])(using Quotes): Expr[t0] = {
    |              leaves.head
    |            }
    |          }
    |${ tconsCase(minSize).indent(5 * indentSize).stripLineEnd }
    |        }
    |      }
    |    }
    |  }
    |}
    |""".stripMargin

  def gen(dir: File): Seq[File] = {
    val file = filename(dir)
    IO.write(file, content)
    Seq(file)
  }

  private def filename(root: File): File = root / "scala"/ "macros" / "tidy.scala"

  private def tconsCase(n: Int): String = {
    if (n <= maxSize) {
      val indices = (0 until n)
      val tupleType = indices.reverse.map(i => s"t$i").mkString("(", ", ", ")")
      val tupleExpr = indices.reverse.map(i => s"$$e$i").mkString("'{ (", ", ", ") }")
      val leavesPattern = indices.map(i => s"LCons(e$i, ").mkString("", "", s"LNil${")" * n}")
      val leavesType = indices.map(i => s"LCons[t$i, ").mkString("", "", s"LNil${"]" * n}")
      s"""|case TCons(given Type[t${n - 1}], tail${n - 1}) => tail${n - 1} match {
          |  case TNil => new BuildFunction[$leavesType, $tupleType] {
          |    override def apply(leaves: $leavesType)(using Quotes): Expr[$tupleType] = {
          |      val $leavesPattern = leaves
          |      $tupleExpr
          |    }
          |  }
          |${ tconsCase(n + 1).indent(indentSize).stripLineEnd }
          |}""".stripMargin
    } else {
      s"""|case _ => {
          |  import quotes.reflect.{Position, report}
          |  report.errorAndAbort("Tuples of size >$maxSize are not supported.", Position.ofMacroExpansion)
          |}""".stripMargin
    }
  }
}
