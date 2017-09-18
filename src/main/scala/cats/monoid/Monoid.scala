package cats.monoid

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}


object Monoid {

  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  val andMonoid = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val orMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
//
//  val addSetMonoid = new Monoid[Set] {
//    override def empty: Set = Set()
//
//    override def combine(x: Set, y: Set): Set = x ++ y
//  }
//
//  val removeSetMonoid = new Monoid[Set] {
//    override def empty: Set = Set()
//
//    override def combine(x: Set, y: Set): Set = x -- y
//  }

}