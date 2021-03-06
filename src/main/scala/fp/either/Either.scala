package fp.either

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case l@Left(_) => l
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case l@Left(_) => l
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => b
      case r@Right(_) => r
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = {
    flatMap(a => b.map(bb => f(a, bb)))
  }

  def map2For[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch { case e: Exception => Left(e) }

  /**
    * The default: => A type annotation indicates that the argument is of type A,
    * but won’t be evaluated until it’s needed by the function
    */
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case h :: t => h.flatMap(a => sequence(t).map(l => l.::(a)))
      case Nil => Right(Nil)
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(b => traverse(t)(f).map(l => l.::(b)))
    }
  }

  def traverse2[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))((b, l) => l.::(b))
    }
  }

  def sequenceViaTraverse[E,A](es: List[Either[E,A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }
}
