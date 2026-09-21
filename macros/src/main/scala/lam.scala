package experiments.macros

import scala.quoted.{Expr, Quotes, Type}

object lam {
  sealed trait Lam[A] {
    def reduce(using Quotes): Lam[A] = this
    def generate(using Quotes): Expr[A]
    final def normalise(using Quotes): Lam[A] = if normal(this) then this else reduce
    final def normaliseGen(using Quotes): Expr[A] = normalise.generate
  }
  case class Abs[A: Type, B: Type](f: Lam[A] => Quotes ?=> Lam[B]) extends Lam[A => B] {
    override def generate(using Quotes): Expr[A => B] = '{ x => ${ f(Var(true, 'x)).normaliseGen } }
  }

  case class App[A: Type, B: Type](f: Lam[A => B], x: Lam[A]) extends Lam[B] {
    override def reduce(using Quotes): Lam[B] = f match {
      case Abs(f0) => f0(x).normalise
      case _      => f.reduce match {
        case Abs(f0) => f0(x).normalise
        case f0      => App(f0, x)
      }
    }

    override def generate(using Quotes): Expr[B] = '{ ${ f.generate }(${ x.normaliseGen }) }
  }

  case class Var[A](simple: Boolean, x: Expr[A]) extends Lam[A] {
    override def generate(using Quotes): Expr[A] = x
  }

  case class If[A: Type](cond: Lam[Boolean], thn: Lam[A], els: Lam[A]) extends Lam[A] {
    override def reduce(using Quotes): Lam[A] = {
      (cond, thn, els) match {
        case (True(), _, _)        => thn
        case (False(), _, _)       => els
        case (_, True(), True())   => True()
        case (_, False(), False()) => False()
        case (_, True(), False())  => cond
        case _                     => If(cond.normalise, thn.normalise, els.normalise).normalise
      }
    }

    override def generate(using Quotes): Expr[A] = {
      (thn, els) match {
        case (True(), _)       => '{ ${ cond.generate } || ${ els.generate } }
        case (_, False())      => '{ ${ cond.generate } && ${ els.generate } }
        case (False(), True()) => '{ !${ cond.generate } }
        case _                 => '{ if ${ cond.generate } then ${ thn.generate } else ${ els.generate } }
      }
    }
  }

  case class Let[A: Type, B: Type](b: Lam[A], i: Lam[A] => Quotes ?=> Lam[B]) extends Lam[B] {
    override def reduce(using Quotes): Lam[B] = {
      b match {
        case v @ Var(true, _) => i(v).normalise
        case _                => this
      }
    }

    override def generate(using Quotes): Expr[B] = {
      '{
        val x = ${ b.normaliseGen }
        ${ i(Var(true, 'x)).normaliseGen }
      }
    }
  }

  case class True()(using Type[Boolean]) extends Lam[Boolean] {
    override def generate(using Quotes): Expr[Boolean] = '{ true }
  }
  case class False()(using Type[Boolean]) extends Lam[Boolean] {
    override def generate(using Quotes): Expr[Boolean] = '{ false }
  }

  def normal[A](lam: Lam[A]): Boolean = lam match {
    case App(Abs(_), _)       => false
    case App(f, _)            => normal(f)
    case If(True(), _, _)       => false
    case If(False(), _, _)      => false
    case If(_, True(), True())    => false
    case If(_, False(), False())  => false
    case If(_, True(), False())   => false
    case If(cond, thn, els)   => normal(cond) && normal(thn) && normal(els)
    case Let(Var(true, _), _) => false
    case _                    => true
  }
}
