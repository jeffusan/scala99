package fp

import org.scalatest.{FlatSpec, Matchers}

class ListSpec  extends FlatSpec with Matchers {

  "dropWhile" should "drop" in {
    val l = List(1, 2, 3)
    def even(x: Int): Boolean = x % 2 == 0
    List.dropWhile(l, even) shouldBe List(1, 3)
  }

  "reverse" should "reverse" in {
    val l = List(1,2,3)
    List.reverse(l) shouldBe List(3,2,1)
  }

  "append" should "append" in {
    val l = List(1,2,3)
    List.append(4, l) shouldBe List(1, 2, 3, 4)
  }

  "creating list" should "create" in {
    val l = List(1,2,3)
    l shouldBe List(1, 2, 3)
  }

}
