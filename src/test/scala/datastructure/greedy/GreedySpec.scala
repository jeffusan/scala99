package datastructure.greedy

import org.scalatest.{FlatSpec, Matchers}

class GreedySpec extends FlatSpec with Matchers {

  "coin change" should "give change" in {
    Greedy.change(List(7,2), 16) shouldBe List(2, 7, 7)
  }

  "coin change" should "be able to backtrack to give change" in {
    Greedy.change(List(5,2), 16) shouldBe List(2, 2, 2, 5, 5)
  }

}
