package training

import org.scalatest.{FlatSpec, Matchers}

class MatchingSetsSpec  extends FlatSpec with Matchers {

  "build string with foldRightViaLeftFold (solution)" should "be in reverse order" in {
    val l = List(1, 2, 3, 4, 5, 6)

    def matching(i: Int, j: Int): Boolean = {
      i % 2 == 0 && j % 2 == 0
    }

    val s = new MatchingSets()

    s.findMatches(l, matching) shouldBe List((2,4),(2, 6),(4,6))
  }

}
