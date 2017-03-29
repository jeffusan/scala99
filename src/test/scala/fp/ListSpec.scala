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

  "concatenate lists" should "create list" in {
    val l = List(List(1,2),List(3,4),List(5,6))
    List.concatenate(l) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "add one" should "create list adding1 to each element" in {
    val l = List(1, 2, 3)
    List.addOne(l) shouldBe List(2, 3, 4)
  }

  "build string with foldLeft" should "be in normal order" in {
    val l = List('a', 'b', 'c')
    List.foldLeft(l, "")((s, e) => s + e) shouldBe "abc"
  }

  "build string with foldRight" should "be in reverse order" in {
    val l = List('a', 'b', 'c')
    List.foldRight(l, "")((e, s) => s + e) shouldBe "cba"
  }

  "build string with foldRightViaLeft" should "be in reverse order" in {
    val l = List('a', 'b', 'c')
    List.foldRightViaLeft(l, "")((e, s) => s + e) shouldBe "cba"
  }

  "build string with foldRightViaLeftFold (solution)" should "be in reverse order" in {
    val l = List('a', 'b', 'c')
    List.foldRightViaFoldLeft_1(l, "")((e, s) => s + e) shouldBe "cba"
  }

}
