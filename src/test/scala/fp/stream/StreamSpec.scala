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

  "take via unfold" should "take n first elements" in {
    val result = Stream(1, 2, 3, 4, 5).takeViaUnfold(3)
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

  "takeWhile via unfold" should "take first elements that match predicate" in {
    val result = Stream(1,2,3,4,5).takeWhileViaUnfold(_ < 4)
    result.toList shouldBe List(1,2,3)
  }

  "takeWhile via fold" should "take first elements that match predicate" in {
    val result = Stream(1,2,3,4,5).takeWhileViaFold(_ < 4)
    result.toList shouldBe List(1,2,3)
  }

  "constant" should "generate infinite stream" in {
    val result = Stream.constant(9).take(3)
    result.toList shouldBe Stream(9,9,9).toList
  }

  "from" should "generate infinite stream starting at n" in {
    val result = Stream.from(3).take(3)
    result.toList shouldBe Stream(3,4,5).toList
  }

  "fibs" should "generate stream of fibonacci numbers" in {
    val result = Stream.fibs().take(7)
    result.toList shouldBe Stream(0,1,1,2,3,5,8).toList
  }

  "fibs via unfold" should "generate stream of fibonacci numbers" in {
    val result = Stream.fibsViaUnfold().take(7)
    result.toList shouldBe Stream(0,1,1,2,3,5,8).toList
  }

  "from via unfold" should "generate infinite stream starting at n" in {
    val result = Stream.fromViaUnfold(3).take(3)
    result.toList shouldBe Stream(3,4,5).toList
  }

  "constant via unfold" should "generate infinite stream" in {
    val result = Stream.constantViaUnfold(9).take(3)
    result.toList shouldBe Stream(9,9,9).toList
  }

  "map" should "itereate over stream" in {
    def plusOne: Int => Int = _ + 1
    val result = Stream(0, 1, 2).map(plusOne)
    result.toList shouldBe List(1,2,3)
  }

  "map via unfold" should "itereate over stream" in {
    def plusOne: Int => Int = _ + 1
    val result = Stream(0, 1, 2).mapViaUnfold(plusOne)
    result.toList shouldBe List(1,2,3)
  }

  "zipWith" should "combine streams and stop as soon as one stops" in {
    val s1 = Stream(1, 2, 3, 4, 5)
    val s2 = Stream(1, 2, 3)
    def f: (Int, Int) => Int = _ + _
    val result = Stream.zipWith(s1, s2)(f)
    result.toList shouldBe List(2, 4, 6)
  }

  "zipAll" should "combine streams and stop as long as there are elements in one of them" in {
    val s1 = Stream(1, 2, 3, 4, 5)
    val s2 = Stream(1, 2, 3)
    val result = s1.zipAll(s2)
    result.toList shouldBe List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (Some(4), None), (Some(5), None))
  }


}
