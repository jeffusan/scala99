package disjointset

import org.scalatest.{FlatSpec, Matchers}

class GraphComponentsSpec extends FlatSpec with Matchers {

  "hackerrank case" should "be correct" in {
    val r = GraphComponents.build(List((1, 6), (2, 7), (3,8), (4,9), (2,6)))
    (r.max, r.min) shouldBe (4, 2)
  }

  "min of size 3 case" should "be correct" in {
    val r = GraphComponents.build(List((1, 6), (2, 7), (3,8), (4,9), (2,6), (4, 5), (8, 10)))
    (r.max, r.min) shouldBe (4, 3)
  }

  "all connected" should "be correct" in {
    val r = GraphComponents.build(List((1,2), (2,3), (3,4), (4,5), (5,6)))
    (r.max, r.min) shouldBe (6, 6)
  }

}
