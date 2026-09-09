package experiments.macros

object utils {
  extension [A] (x: A) {
    def some: Option[A] = Some(x)
    def asLeft[B]: Either[A, B] = Left(x)
    def asRight[B]: Either[B, A] = Right(x)
  }

  extension [A, B] (x: Either[A, B]) {
    def bimap[C, D](f: A => C, g: B => D): Either[C, D] = x match {
      case Left(value)  => Left(f(value))
      case Right(value) => Right(g(value))
    }
  }

  extension [A, B] (x: (A, B)) {
    def bimap[C, D](f: A => C, g: B => D): (C, D) = (f(x._1), g(x._2))
  }
}
