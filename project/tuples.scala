import sbt.*

object tuples {

  def gen(dir: File): Seq[File] = {
    val file = filename(dir)
    IO.write(file, content)
    Seq(file)
  }

  def filename(root: File): File = root / "scala"/ "macros" / "tidy.scala"

  val content = s"""|package experiments.macros
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
    |${ tupleCase(2).indent(8).stripLineEnd }
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
        s"""|case TCons(given Type[t${n - 1}], tail${n - 1}) => tail${n - 1} match {
            |  case TNil => new BuildFunction[ToLeaves, $tupleType] {
            |    override def apply(leaves: ToLeaves)(using Quotes): Expr[$tupleType] = {
            |      val $leavesPattern = leaves.asInstanceOf[$leavesType]
            |      $tupleExpr
            |    }
            |  }
            |${tupleCase(n + 1).indent(2).stripLineEnd}
            |}""".stripMargin
      } else {
        "case _ => ???"
      }
    }
}
