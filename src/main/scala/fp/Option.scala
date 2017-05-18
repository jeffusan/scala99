package fp

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
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
