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

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (s, i) => s.headOption().flatMap(
      h => if(i > 0) Some(h, (s.drop(1), i -1))
           else None)
    case _ => None
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

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) =>  Some(h(), t())
    case _ => None
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

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this)(s => s.headOption().map(h => (f(h), s.drop(1))))

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

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s2) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case _ => None
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  /**
    * It takes an initial state, and a function for producing both the next state
    * and the next value in the generated stream
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(i => cons(i._1, unfold(i._2)(f))).getOrElse(empty)
  }

  val onesViaFold: Stream[Int] = unfold(1)(_ => Some((1,1)))

  def fibsViaUnfold(): Stream[Int] = unfold((0,1)) {
    s => Some(s._1, (s._2, s._1 + s._2))
  }

  def fibsViaUnfold2: Stream[Int] =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def zipWith[A, B,C](l: Stream[A], k: Stream[B])(f: (A,B) => C): Stream[C] = unfold(l,k){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }
}
