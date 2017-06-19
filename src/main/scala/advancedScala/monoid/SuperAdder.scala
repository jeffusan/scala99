package advancedScala.monoid

import cats.instances.int._
import cats.instances.set._
import cats.kernel.Monoid

object SuperAdder {

  val intSetMonoid = Monoid[Set[Int]]

  def add(items: List[Int]): Int = {
    items.foldRight(Monoid[Int].empty)((a,b) => Monoid[Int].combine(a, b))
  }

}
