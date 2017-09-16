package disjointset

import org.scalatest.{FlatSpec, Matchers}

class ComponentSizesSpec extends FlatSpec with Matchers {

  "hackerrank case" should "be correct" in {
    val r = ComponentSizes.build(5, List((1, 6), (2, 7), (3,8), (4,9), (2,6)))
    (r._2, r._1) shouldBe (4, 2)
  }

  "min of size 3 case" should "be correct" in {
    val r = ComponentSizes.build(5, List((1, 6), (2, 7), (3,8), (2,6), (3, 10)))
    (r._2, r._1) shouldBe (4, 3)
  }

  "all connected" should "be correct" in {
    val r = ComponentSizes.build(5, List((1,6), (2,7), (3,8), (1,7), (2,8)))
    (r._2, r._1) shouldBe (6, 6)
  }

  "none connected" should "be correct" in {
    val r = ComponentSizes.build(5, List((1,5), (2,6), (3,7)))
    (r._2, r._1) shouldBe (2, 2)
  }

  "min of size 3 case including single node" should "ignore single and be correct" in {
    val r = ComponentSizes.build(5, List((1, 6), (2, 7), (3,8), (2,6), (3, 10), (2, 2)))
    (r._2, r._1) shouldBe (4, 3)
  }

  "min of size 3 case including outside of range: x > N" should "ignore single and be correct" in {
    val r = ComponentSizes.build(5, List((1, 6), (2, 7), (3,8), (2,6), (3, 10), (6, 7)))
    (r._2, r._1) shouldBe (4, 3)
  }

  "min of size 2 case, max 9" should "be correct" in {
    val r = ComponentSizes.build(10, List((1, 11), (2, 12), (3,13), (4,14), (5, 11), (6, 12), (6, 13), (1, 13), (3, 19)))
    (r._2, r._1) shouldBe (9, 2 )
  }

  "min of size 8 case, max 8" should "be correct" in {
    val r = ComponentSizes.build(10, List((1, 11), (2, 12), (3,13), (5, 11), (6, 12), (6, 13), (1, 13)))
    (r._2, r._1) shouldBe (8, 8)
  }

}
