package leet

import org.scalatest.{FlatSpec, Matchers}

class SlidingWindowMaxSpec extends FlatSpec with Matchers {

  "basic sliding window" should "be correct" in {
    SlidingWindowMax.maxSlidingWindow(Array(1,3,-1,-3,5,3,6,7), 3) shouldBe Array(3,3,5,5,6,7)
  }

  "basic sliding window" should "window same size as array" in {
    SlidingWindowMax.maxSlidingWindow(Array(1,3,-1,-3,5,3,6,7), 8) shouldBe Array(7)
  }

}
