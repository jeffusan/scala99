package datastructure.advancedList

import datastructure.advancedLists.Binary
import org.scalatest.{FlatSpec, Matchers}

class BinarySpec extends FlatSpec with Matchers {

  "adding" should "handle carrying" in {
    val a = List(0,1,1,1,1) // 30
    val b = List(1,1,0,1,0) // 11
    Binary.add(a, b) shouldBe List(1, 0, 0, 1, 0, 1)
  }

}
