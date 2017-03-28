package fp

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](in: List[A]): List[A] = in match {
    case Nil => sys.error("tail of empty list")
    case Cons(x, tail) => tail
  }

  def setHead[A](head: A, l: List[A]): List[A] = {
    Cons(head, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case Cons(head, tail) => Cons(head, dropWhile(tail, f))
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(last, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
      def l(as: List[A], c: Int): Int = as match {
        case Nil => c
        case Cons(h, t) => l(t, c + 1)
      }
    l(as, 0)
  }

  def length2[A](as: List[A]): Int = foldRight(as, 0)((x,y) => y + 1)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => x + 1)

  def reverse[A](in: List[A]): List[A] = {
    foldLeft(in, List[A]())((result, h) => Cons(h, result))
  }

  def append[A](a: A, l: List[A]): List[A] = {
    foldLeft(reverse(l), Cons(a, Nil))((r, e) => Cons(e, r))
  }



}
