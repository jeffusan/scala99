package datastructure

object List {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class ::[+A](head: A, tail: List[A]) extends List[A]

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))
  }

}