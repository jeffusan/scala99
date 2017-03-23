package fp

import org.scalatest.{FlatSpec, Matchers}

class ListSpec  extends FlatSpec with Matchers {

  "dropWhile" should "drop" in {

    val l = List(1, 2, 3)
    def even(x: Int): Boolean = x % 2 == 0

    List.dropWhile(l, even) shouldBe List(1, 3)


  }

}
