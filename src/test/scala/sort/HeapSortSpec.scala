package sort

import org.scalatest.{FlatSpec, Matchers}

class HeapSortSpec extends FlatSpec with Matchers {

  it should "sort" in {
    HeapSort.sort(Array(1,3,6,7,2,9,5,4,8)) shouldBe Array(1,2,3,4,5,6,7,8,9)
  }

}
