package advancedScala.monad

import scala.language.higherKinds


/**
  * Importantly, the pure and flatMap methods must obey three laws:
  * Left identity: calling pure then transforming the result with a func on f is the same as simply calling f:
  *     pure(a).flatMap(f) == f(a)
  * Right identity: passing pure to flatMap is the same as doing nothing:
  *     m.flatMap(pure) == m
  * Associativity: flatMapping over two func ons f and g is the same as flatMapping over f and then flatMapping over g:
  *     m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  */
trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}
