package interviews

import org.scalatest.{FlatSpec, Matchers}

class SmallestGroupSpec extends FlatSpec with Matchers {

  "finding smallest group" should "work" in {

//    {
//      {1, 6},
//      {2, 7},
//      {3, 8},
//      {4, 9},
//      {3, 5},
//      {2, 6}
//    };
    val in = Array((1, 6), (2, 7), (3,8), (4,9), (3, 5), (2, 6))
    SmallestGroup.smallestGroup(in) shouldBe 2

    val in2 = Array((1, 6), (2, 7), (3,8), (3, 5), (2, 6))
    SmallestGroup.smallestGroup(in2) shouldBe 3

    val in3 = Array((1, 6), (2, 7), (2, 6))
    SmallestGroup.smallestGroup(in3) shouldBe 4
  }

  "foldLeft on Nil" should "return init value" in {
    val list: List[Int] = Nil
    val init = 8
    list.foldLeft(init)((_,_) ⇒ 9) shouldBe init
  }

}
