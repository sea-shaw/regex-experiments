package experiments.cps.macros

import scala.quoted.{Expr, FromExpr, Quotes}

object config {
  /* Can't use default paramters here as they are not inlined. */
  case class Config(ignoreEmpty: Boolean)

  given FromExpr[Config] {
    override def unapply(config: Expr[Config])(using Quotes): Option[Config] = {
      config match {
        case '{ Config(${ Expr(ignoreEmpty) }) } => Some(Config(ignoreEmpty))
        case _                                   => None
      }
    }
  }

  /* If this isn't transparent then it isn't inlined in calls to `r`. I think
     it's because trasnparent inlines are inlined before typechecking and normal
     inlines are inlined after. */ 
  transparent inline given Config = Config(false)
}
