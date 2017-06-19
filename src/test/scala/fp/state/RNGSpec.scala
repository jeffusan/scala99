package fp.state

import fp.state.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers {

  val base = SimpleRNG(42)
  "RNG.nonNegative" should "return a non negative random int" in {
    val (i, _) = RNG.nonNegativeInt(base)
    assert(i > 0)
  }

  "RNG double" should "return a double between 0 and 1" in {
    val (d, _ ) = RNG.double(base)
    assert(d >= 0)
    assert(d <= 1)
  }

}
