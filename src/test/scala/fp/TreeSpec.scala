package fp

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "leaf size" should "be 1" in {
    val t = Leaf(0)
    Tree.size(t) shouldBe 1
  }

  "tree height 2" should "be 3" in {
    val t = Branch(Leaf(0), Leaf(0))
    Tree.size(t) shouldBe 3
  }

  "tree of height 3" should "be 7" in {
    val t = Branch(Branch(Leaf(0), Leaf(0)), Branch(Leaf(0), Leaf(0)))
    Tree.size(t) shouldBe 7
  }

  "max[Int] in tree" should "be 7" in {
    val t = Branch(Branch(Leaf(0), Leaf(7)), Branch(Leaf(5), Leaf(2)))
    Tree.max(t) shouldBe 7
  }

  "max[Int] in bigger tree" should "be 10" in {
    val t = Branch(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(2), Leaf(7))), Branch(Branch(Leaf(10), Leaf(1)), Branch(Leaf(5), Leaf(7))))
    Tree.max(t) shouldBe 10
  }

}
