package ar.empanada

import cats.effect.IO

object Exercise1 extends App {
// show Fluffy instances
  trait Fluffy[F[_]] {
    def furry[A, B](fa: F[A])(f: A => B): F[B]
  }

// Exercise 1
// Relative Difficulty: 1
  implicit def fluffyList: Fluffy[List] = new Fluffy[List] {
    override def furry[A, B](fa: List[A])(f: A => B): List[B] =
      fa.foldRight(List.empty[B])((curr, acc) => f(curr) :: acc)
  }

// Exercise 2
// Relative Difficulty: 1
  implicit def fluffyOption: Fluffy[Option] = new Fluffy[Option] {
    override def furry[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.foldRight(Option.empty[B])((curr, _) => Option(f(curr)))
  }

// Exercise 3
// Relative Difficulty: 5
  implicit def fluffyReader[T]: Fluffy[T => *] = new Fluffy[T => *] {
    override def furry[A, B](fa: T => A)(f: A => B): T => B =
      t => f(fa(t))
  }

// Exercise 4
// Relative Difficulty: 5
  implicit def fluffyEither[L]: Fluffy[Either[L, *]] =
    new Fluffy[Either[L, *]] {
      override def furry[A, B](fa: Either[L, A])(f: A => B): Either[L, B] =
        fa match {
          case Left(l)  => Left[L, B](l)
          case Right(a) => Right[L, B](f(a))
        }
    }
}
