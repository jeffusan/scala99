package search

import org.scalatest.{FlatSpec, Matchers}

class ConnectedCellsSpec extends FlatSpec with Matchers {

  "counting regions" should "be correct" in {
    val grid = Array(
      Array(1,0,1,0,0,1),
      Array(0,1,0,0,0,0),
      Array(0,0,0,1,1,0),
      Array(0,0,0,0,1,0),
      Array(1,1,1,0,0,0),
      Array(0,0,1,0,0,0))

    ConnectedCells.analyse(grid) shouldBe (RegionCount(4), MaxSize(4))
  }

}
