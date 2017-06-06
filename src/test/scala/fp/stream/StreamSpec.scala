package fp.stream

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {


  private def a3 = () => {3}
  private def a2 = () => {2}
  private def a1 = () => { 1}

  "toList" should "toList from direct case class" in {
    val s = Cons(a1, () => Cons(a2, () => Cons(a3, () => Empty)))
    val result = s.toList
    result shouldBe List(1, 2, 3)

  }

  "toList" should "toList from object memoization" in {

    Stream.apply(1,2,3).toList shouldBe List(1,2,3)
  }

  "take" should "take n first elements" in {
    val result = Stream(1, 2, 3, 4, 5).take(3)
    result.toList shouldBe List(1, 2, 3)
  }

  "drop" should "drop n first elements" in {
    val result = Stream(1,2,3,4,5).drop(3)
    result.toList shouldBe List(4,5)
  }

  "takeWhile" should "take first elements that match predicate" in {
    val result = Stream(1,2,3,4,5).takeWhile(_ < 4)
    result.toList shouldBe List(1,2,3)
  }


}
