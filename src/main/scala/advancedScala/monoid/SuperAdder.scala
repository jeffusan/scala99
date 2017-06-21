package advancedScala.monoid

import cats.instances.set._

import cats.Monoid
import cats.syntax.semigroup._

object SuperAdder {

  val intSetMonoid = Monoid[Set[Int]]

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  case class Order(totalCost: Double, quantity: Double)

  implicit val addOrderMonoid = new Monoid[Order] {
    def empty: Order = Order(0, 0)
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}
