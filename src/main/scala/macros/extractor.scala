package experiments.macros

import scala.compiletime.ops.int.{S, +}

object extractor {
  object Alt {
    def unapply[I <: Int, A, B](alt: A)(using extractor: Extractor[I, A, B]): (Fin[I], B) = extractor.extract(alt)
  }

  type Fin[I <: Int] <: Int = I match {
    case 0 => Nothing
    case S[i] => Fin[i] | i
  }

  sealed trait Extractor[I <: Int, A, B] {
    val count: I
    def extract(alt: A): (Fin[I], B)
  }

  sealed trait LowPriorityExtractor {
    given [A] => Extractor[1, A, A] {
      override val count: 1 = 1
      override def extract(alt: A): (0, A) = (0, alt)
    }
  }

  object Extractor extends LowPriorityExtractor {
    given [I <: Int, J <: Int, A, B, C] => (left: Extractor[I, A, C], right: Extractor[J, B, C]) => Extractor[I + J, Either[A, B], C] = new {
      override val count: I + J = plus(left.count, right.count)
      override def extract(alt: Either[A, B]): (Fin[I + J], C) = alt match {
        case Left(a)  => {
          val (i, c) = left.extract(a)
          (widenFin[I, J](i), c)
        }
        case Right(b) => {
          val (i, c) = right.extract(b)
          val j = plusFin(left.count, i)
          (j, c)
        }
      }
    }
  }

  private def plus[I <: Int, J <: Int](i: I, j: J): I + J = (i + j).asInstanceOf[I + J]
  private def plusFin[I <: Int, J <: Int](i: I, fin: Fin[J]): Fin[I + J] = (i + fin).asInstanceOf[Fin[I + J]]
  private def widenFin[I <: Int, J <: Int](fin: Fin[I]): Fin[I + J] = fin.asInstanceOf[Fin[I + J]]
}
