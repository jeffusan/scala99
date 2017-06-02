package scala.sequences

import org.scalatest.{FlatSpec, Matchers}
import sequences.LongestSequence

class LongestSequenceSpec extends FlatSpec with Matchers {

  "longest sequence" should
    "find longuest increasing sequence" in {
    val result = LongestSequence.longestSequence(Seq(0,8,4,12, 2,10,6,14,1,9,5,13,3,11,7,15))
    result shouldBe Seq(0,2,6,9,11,15)
  }

  "already increasing sequence" should
    "return input untouched" in {
    val result = LongestSequence.longestSequence(Seq(0,1,2,3,4,5))
    result shouldBe Seq(0,1,2,3,4,5)
  }


}
