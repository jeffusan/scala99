package advancedScala.functors

import cats.Functor
import cats.syntax.functor._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]


object TreeSyntax {

  implicit class FunctorOps[A](value: Tree[A]) {
    def map[B](f: (A) => B)(implicit p: Functor[Tree]): Tree[B] = {
      p.map(value)(f)
    }

  }
}

object Tree {

  implicit val treeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }
}
