package experiments.macros

import scala.compiletime.ops.int.{S, +}

object extractor {

  object extract {
    def unapply[I <: Int, A, B](x: A)(using extractor: Extractor[I, A, B]): (Fin[I], B) = extractor.unapply(x)
  }

  type Fin[I <: Int] <: Int = I match {
    case 0 => Nothing
    case S[i] => Fin[i] | i
  }

  sealed trait Extractor[I <: Int, -A, +B] {
    val i: I
    def unapply(x: A): (Fin[I], B)
  }

  sealed trait LowPriorityExtractor {
    given [A] => Extractor[1, A, A] {
      override val i: 1 = 1
      override def unapply(x: A): (0, A) = (0, x)
    }
  }

  object Extractor extends LowPriorityExtractor {
    given [I <: Int, J <: Int, A, B, C] => (aExtractor: Extractor[I, A, C], bExtractor: Extractor[J, B, C]) => Extractor[I + J, Either[A, B], C] = new {
      override val i: I + J = plus(aExtractor.i, bExtractor.i)
      override def unapply(x: Either[A, B]): (Fin[I + J], C) = x match {
        case Left(a)  => {
          val (i, c) = aExtractor.unapply(a)
          (i.asInstanceOf[Fin[I + J]], c)
        }
        case Right(b) => {
          val (i, c) = bExtractor.unapply(b)
          val j = plus(aExtractor.i, i).asInstanceOf[Fin[I + J]]
          (j, c)
        }
      }
    }
  }
  private def plus[I <: Int, J <: Int](i: I, j: J): I + J = (i + j).asInstanceOf[I + J]
}
