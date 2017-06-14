package advancedScala.monoids

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  val empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid

  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  def andSemigroup = new Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  def orSemigroup = new Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  def andMonoid = new Monoid[Boolean] {
    override val empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = andSemigroup.combine(x, y)
  }

  def orMonoid = new Monoid[Boolean] {
    override val empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = orSemigroup.combine(x, y)
  }
}
