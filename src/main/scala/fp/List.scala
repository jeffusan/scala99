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

  def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

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

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldLeft(l, List[A]())((r, list) => foldLeft(list, r)((re, a) => append(a, re)))
  }

  def addOne(l: List[Int]): List[Int] = {
    foldLeft(l, List[Int]())((l, e) => append(e +1 , l))
  }

  /**
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String
    */
  def doubleListToString(l: List[Double]): List[String] = {
    List.foldLeft(l, List[String]())((r,d) => append(d.toString, r))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    List.foldLeft(reverse(as), List[B]())((l, e) => Cons(f(e), l))
  }

  def doubleListToStringWithMap(l: List[Double]): List[String] = {
    map(l)(_.toString)
  }

  /**
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int].
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(reverse(as), List[A]())((l,e) => if (f(e)) { Cons(e, l) } else { l } )
  }

  /**
    * Write a function flatMap that works like map except that the function given
    * will return a list instead of a single result, and that list should be inserted into the final resulting list.
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(reverse(as), List[B]())((l,e) => foldLeft(reverse(f(e)), l)((tl, te) => Cons(te, tl)))
  }




}
