package experiments.cps.macros

import scala.quoted.{Expr, FromExpr, Quotes, quotes}

object config {
  val defaultIgnoreEmpty = false
  val defaultTailRecursive = true

  case class Config(
    ignoreEmpty: Boolean = defaultIgnoreEmpty,
    tailRecursive: Boolean = defaultTailRecursive,
  )

  /* If this isn't transparent then it isn't inlined in calls to `r`. I think
    it's because trasnparent inlines are inlined before typechecking and normal
    inlines are inlined after. */ 
  transparent inline given Config = Config()

  given FromExpr[Config] {
    override def unapply(config: Expr[Config])(using Quotes): Option[Config] = {
      config match {
        /* Can't specify parameters in the match since then it won't match
           default parameters. Need to ensure all parameters are matched and in
           the correct order. */
        case '{ Config($ignoreEmptyExpr, $tailRecursiveExpr) } => {
          for {
            ignoreEmpty <- parameterValue(ignoreEmptyExpr, 1, defaultIgnoreEmpty)
            tailRecursive <- parameterValue(tailRecursiveExpr, 2, defaultTailRecursive)
          } yield Config(
            ignoreEmpty = ignoreEmpty,
            tailRecursive = tailRecursive,
          )
        }
        case _ => None
      }
    }
  }

  def parameterValue[A: FromExpr](expr: Expr[A], defaultNumber: Int, defaultValue: A)(using Quotes): Option[A] = expr match {
    case Expr(value)                               => Some(value)
    case DefaultParameter(n) if n == defaultNumber => Some(defaultValue)
    case _                                         => None
  }

  object DefaultParameter {
    def unapply(expr: Expr[?])(using Quotes): Option[Int] = {
      import quotes.reflect.{Ident, Select, asTerm}
      expr.asTerm match {
        case Select(Ident("Config"), s"$$lessinit$$greater$$default$$$n") => n.toIntOption
        case _                                                             => None
      }
    }
  }
}
