package cats.monoidadder

object MiscMonoid {

  import cats.Monoid
  import cats.syntax.semigroup._

  implicit val multiplicationMonoid =
    new Monoid[Int] {
      def empty: Int = 1
      override def combine(x: Int, y: Int): Int = x * y
    }

  3 |+| 2

}
