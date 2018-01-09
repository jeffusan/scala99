package fp.testing

import fp.state.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {

  "string" should "generate random string" in {

    val r = SimpleRNG(123456)
    val (a, r2) = Gen.string(10).sample.run(r)
    println(a)
    val (b, _) = Gen.string(10).sample.run(r2)
    println(b)
    assert(a != b)
  }

}
