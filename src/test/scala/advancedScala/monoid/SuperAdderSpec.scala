package advancedScala.monoid

import cats.instances.int._

import org.scalatest.{FlatSpec, Matchers}

class SuperAdderSpec extends FlatSpec with Matchers {

  "intSetMonoid" should "combine" in {
    val result = SuperAdder.intSetMonoid.combine(Set(1, 2), Set(2, 3))
    result shouldBe Set(1, 2, 3)
  }

  "add" should "add using monoids from Cats lib" in {
    val result = SuperAdder.add(List(1, 2, 3))
    result shouldBe 6
  }

}
