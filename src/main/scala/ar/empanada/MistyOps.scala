package ar.empanada

object MistyOps extends App {
  // show Additional Misty functions
  trait Misty[F[_]] {
    def banana[A, B](fa: F[A])(f: A => F[B]): F[B]
    def unicorn[A](a: A): F[A]
  }

// Exercise 6
// Relative Difficulty: 3
// (use banana and/or unicorn)
  def furry_[F[_], A, B](fa: F[A])(f: A => B)(implicit misty: Misty[F]): F[B] =
    misty.banana(fa)(a => misty.unicorn(f(a)))

// Exercise 12
// Relative Difficulty: 3
  def jellybean[F[_], A](ffa: F[F[A]])(implicit misty: Misty[F]): F[A] =
    misty.banana(ffa)(identity)

// Exercise 13
// Relative Difficulty: 6
  def apple[F[_], A, B](fa: F[A])(fab: F[A => B])(implicit
      misty: Misty[F]
  ): F[B] =
    misty.banana(fab)(ab => misty.banana(fa)(a => misty.unicorn(ab(a))))

  def apple_[F[_], A, B](fa: F[A])(fab: F[A => B])(implicit
      misty: Misty[F]
  ): F[B] =
    misty.banana(fab)(furry_(fa))

// Exercise 14
// Relative Difficulty: 6
  def moppy[F[_], A, B](la: List[A])(f: A => F[B])(implicit
      misty: Misty[F]
  ): F[List[B]] =
    la.foldRight[F[List[B]]](misty.unicorn(List.empty[B]))((curr, acc) =>
      misty.banana(acc)(list =>
        misty.banana(f(curr))(b => misty.unicorn(b :: list))
      )
    )

// Exercise 15
// Relative Difficulty: 6
// (bonus: use moppy)
  def sausage[F[_], A](lfa: List[F[A]])(implicit misty: Misty[F]): F[List[A]] =
    moppy(lfa)(identity)

// Exercise 16
// Relative Difficulty: 6
// (bonus: use apple + furry_)
  def banana2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit
      misty: Misty[F]
  ): F[C] =
    apple(
      misty.banana(fa)(a => furry_(fb)(b => (a, b)))
    )(misty.unicorn(f.tupled))

// Exercise 17
// Relative Difficulty: 6
// (bonus: use apple + banana2)
  def banana3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D
  )(implicit misty: Misty[F]): F[D] =
    apple(
      misty.banana(fa)(a =>
        misty.banana(fb)(b => misty.banana(fc)(c => misty.unicorn(a, b, c)))
      )
    )(misty.unicorn(f.tupled))

  def banana3_[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D
  )(implicit misty: Misty[F]): F[D] =
    apple(
      misty.banana(fc)(c => banana2(fa, fb)((a, b) => (a, b, c)))
    )(misty.unicorn(f.tupled))

  // Exercise 18
// Relative Difficulty: 6
// (bonus: use apple + banana3)
  def banana4[F[_], A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  )(implicit misty: Misty[F]): F[E] =
    apple(
      misty.banana(fd)(d => banana3(fa, fb, fc)((a, b, c) => (a, b, c, d)))
    )(misty.unicorn(f.tupled))
}
