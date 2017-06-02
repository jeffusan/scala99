package fp.options

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "map2" should "extract both options" in {
    def f: (Int, Int) => Int = _ + _
    Some("").map2(Some(1), Some(1))(f) shouldBe Some(2)
  }

  "map2For" should "extract both options" in {
    def f: (Int, Int) => Int = _ + _
    Some("").map2For(Some(1), Some(1))(f) shouldBe Some(2)
  }

}
