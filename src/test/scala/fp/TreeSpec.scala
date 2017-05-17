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

  "depth of a leaf" should "be 1" in {
    val t = Leaf(1)
    Tree.depth(t) shouldBe 1
  }

  "depth of a 3 tree" should "be 3" in {
    val t = Branch(Branch(Leaf(0), Leaf(7)), Branch(Leaf(5), Leaf(2)))
    Tree.depth(t) shouldBe 3
  }

  "depth of a tree with a left branch deeper than right" should "be depth of left" in {
    val t = Branch(Branch(Branch(Leaf(0), Leaf(7)), Leaf(1)), Leaf(2))
    Tree.depth(t) shouldBe 4
  }

  "map on a leaf" should "apply to the value" in {
    def f: Int => Int = _ + 1
    val t = Leaf(1)
    Tree.map(f, t) shouldBe Leaf(2)
  }

  "map on a 3 tree" should "be apply on leaves" in {
    def f: Int => Int = _ + 1
    val t = Branch(Branch(Leaf(0), Leaf(7)), Branch(Leaf(5), Leaf(2)))
    Tree.map(f, t) shouldBe Branch(Branch(Leaf(1), Leaf(8)), Branch(Leaf(6), Leaf(3)))
  }

}
