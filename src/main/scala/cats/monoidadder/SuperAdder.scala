package cats.monoidadder

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._

object SuperAdder {

  def add(items: List[Int]): Int = {
    items.foldLeft(Monoid[Int].empty)(_ |+| _)
  }

}
