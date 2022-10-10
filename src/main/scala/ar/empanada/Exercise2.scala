package ar.empanada

object Exercise2 extends App {
  // show Misty instances
  trait Misty[F[_]] {
    def banana[A, B](fa: F[A])(f: A => F[B]): F[B]
    def unicorn[A](a: A): F[A]
  }

// Exercise 7
// Relative Difficulty: 2
  implicit def mistyList: Misty[List] = new Misty[List] {
    override def banana[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.foldRight(List.empty[B])((curr, acc) => acc ++ f(curr))

    override def unicorn[A](a: A): List[A] =
      if (a == null) List.empty[A]
      else List(a)
  }

// Exercise 8
// Relative Difficulty: 2
  implicit def mistyList2: Misty[Option] = new Misty[Option] {
    override def banana[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.foldRight(Option.empty[B])((curr, _) => f(curr))

    override def unicorn[A](a: A): Option[A] =
      Option(a)
  }

// Exercise 9
// Relative Difficulty: 6
  implicit def mistyReader[T]: Misty[T => *] = new Misty[T => *] {
    override def banana[A, B](fa: T => A)(f: A => T => B): T => B =
      t => f(fa(t))(t)

    override def unicorn[A](a: A): T => A =
      t => a
  }

// Exercise 10
// Relative Difficulty: 6
  implicit def mistyEither[L]: Misty[Either[L, *]] = new Misty[Either[L, *]] {
    override def banana[A, B](fa: Either[L, A])(
        f: A => Either[L, B]
    ): Either[L, B] =
      fa match {
        case Left(value) => Left[L, B](value)
        case Right(a)    => f(a)
      }

    override def unicorn[A](a: A): Either[L, A] =
      Right[L, A](a)
  }
}
