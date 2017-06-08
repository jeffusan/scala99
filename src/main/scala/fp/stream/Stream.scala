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

  /**
    * f is non-strict in its second parameter.
    * If f chooses not to evaluate its sec- ond parameter, this terminates the traversal early
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
    * b is the unevaluated recursive step that folds the tail of the stream.
    * If p(a) returns true, b will never be evaluated and the computation terminates early
    */
  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a,b) => p(a) || b)
  }

  /**
    * checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a nonmatching value
    */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a,b) => p(a) && b)
  }

  def takeWhileViaFold(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) =>
      if(p(a)) cons(a,b)
      else empty
    )
  }

  def headOptionViaFold(): Option[A] = {
    foldRight(Option.empty[A])((a,_) => Some(a))
  }

  def headOptionViaFold_sol(): Option[A] = {
    foldRight(None: Option[A])((a,_) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a,b) => cons(f(a),b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) =>
      if(f(a)) cons(a, b)
      else b
    )
  }

  /**
    * https://twitter.github.io/scala_school/type-basics.html
    * if B < A cons(a, b) is a A which can be != from B
    * with A < B, the cons(a,b) is a A which is of return type B
    */
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h,t) => f(h).append(t))
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n +1 ))

  def fibs(): Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(i, go(j, i +j))
    }
    go(0,1)
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    
  }
}
