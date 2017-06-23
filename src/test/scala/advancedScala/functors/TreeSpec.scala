package advancedScala.functors

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  import Tree._
  import TreeSyntax._

  "tree functor" should "map all the way to leaves" in {
    val result = Branch(Leaf(10), Leaf(20)).map(_ * 2)
    result shouldBe Branch(Leaf(20), Leaf(40))
  }


}
