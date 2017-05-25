package fp.options

import fp.datastructure

trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  /** Returns this $option if it is nonempty,
  *  otherwise return the result of evaluating `alternative`.
  */
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  /**
    * Convert Some to None if the value doesn’t satisfy f.
    */
  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false)) this
    else None
  }

  def filter2(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) this else None)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(someA => b.map(someB => f(someA, someB)))
  }

  def sequence0[A](a: Option[A]): Option[datastructure.List[A]] = {
    a.map(datastructure.List(_))
  }

  def sequence1[A](a: Option[A], b: Option[A]): Option[datastructure.List[A]] = {
    a.flatMap { aa =>
      val l = datastructure.List(aa)
      b.map(bb => datastructure.List.append(bb, l))
    }
  }

  def sequence2[A](a: Option[A], b: Option[A], c: Option[A]): Option[datastructure.List[A]] = {
    a.flatMap { aa =>
      val l = datastructure.List(aa)
      b.flatMap{ bb =>
        val ll = datastructure.List.append(bb, l)
        c.map(cc => datastructure.List.append(cc, ll))
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap { hh => sequence(t).map(l => l.::(hh))}

  }

  def traverse0[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a.map(f))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(fh => traverse(t)(f).map(l => l.::(fh)))
    }
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a)
  }

  def Try[A](a: A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
