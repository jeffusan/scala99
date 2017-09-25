package cats.monad

import cats.Monad

object CustomMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(f: (A) ⇒ Tree[B]): Tree[B] = {
        tree match {
          case Leaf(v) ⇒ f(v)
          case Branch(l,r) ⇒ Branch(flatMap(l)(f), flatMap(r)(f))
        }
    }

    def tailRecM[A, B](a: A)(f: (A) ⇒ Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Leaf(Left(aa)) ⇒ tailRecM(aa)(f)
        case Leaf(Right(b)) ⇒ Leaf(b)
        case Branch(l, r) ⇒ Branch(
          flatMap(l) {
            case Left(ll) ⇒ tailRecM(ll)(f)
            case Right(rr) ⇒ pure(rr)
          },
          flatMap(r) {
            case Left(ll) ⇒ tailRecM(ll)(f)
            case Right(rr) ⇒ pure(rr)
          }
        )

      }
    }

    def pure[A](x: A): Tree[A] = Leaf(x)
  }

  def main(args: Array[String]): Unit = {

    import cats.syntax.functor._
    import cats.syntax.flatMap._

    val t = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

    println(t)
  }

}
