package sequences

import org.scalatest.{FlatSpec, Matchers}

class LongestSequenceSpec extends FlatSpec with Matchers {

  "longest sequence" should
    "find longuest increasing sequence" in {
    val result = LongestSequence.longestSequence(Array(0,8,4,12, 2,10,6,14,1,9,5,13,3,11,7,15))
    result shouldBe Seq(0,2,6,9,11,15)
  }

  "already increasing sequence" should
    "return input untouched" in {
    val result = LongestSequence.longestSequence(Array(0,1,2,3,4,5))
    result shouldBe Seq(0,1,2,3,4,5)
  }

  "hackerrank sequence" should
    "be correct" in {
    val result = LongestSequence.longestSequence(Array(5,2,7,4,3,8))
    result shouldBe Seq(2,3,8)
  }

  "sequence with duplicates" should
    "be correct" in {
    val result = LongestSequence.longestSequence(Array(0,0))
    result shouldBe Seq(0)
  }

  "sequence with duplicates 2" should
    "be correct" in {
    val result = LongestSequence.longestSequence(Array(2,1,2,4,0))
    result shouldBe Seq(1,2,4)
  }

  "sequence with duplicates 3" should
    "be correct" in {
    val result = LongestSequence.longestSequence(Array(2,1,2,4,0,1))
    result shouldBe Seq(1,2,4)
  }

}
