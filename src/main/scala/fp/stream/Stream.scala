package fp.stream

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def headOption(): Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {

    @tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h,t) => go(t(), h() :: l)
    }
    go(this, List()).reverse
  }

  def toListRec: List[A] = {
    this match {
      case Empty => List[A]()
      case Cons(h,t) => h() :: t().toListRec
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
    * returns all starting elements of a Stream that match the given predicate
    */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }



}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
