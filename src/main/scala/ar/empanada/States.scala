package ar.empanada

object States extends App {
  // show State instances for Fluffy and Misty
  trait Misty[F[_]] {
    def banana[A, B](fa: F[A])(f: A => F[B]): F[B]
    def unicorn[A](a: A): F[A]
  }

  trait Fluffy[F[_]] {
    def furry[A, B](fa: F[A])(f: A => B): F[B]
  }

  case class State[S, A](
      state: S => (S, A)
  )

// Exercise 19
// Relative Difficulty: 9
  implicit def fluffyState[S]: Fluffy[State[S, *]] = new Fluffy[State[S, *]] {
    override def furry[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      State { s =>
        val state = fa.state(s)
        (state._1, f(state._2))
      }
  }

// Exercise 20
// Relative Difficulty: 10
  implicit def mistyState[S]: Misty[State[S, *]] = new Misty[State[S, *]] {
    override def banana[A, B](fa: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] =
      State { s =>
        val state = fa.state(s)
        f(state._2).state(state._1)
      }

    override def unicorn[A](a: A): State[S, A] =
      State { s => (s, a) }
  }
}
