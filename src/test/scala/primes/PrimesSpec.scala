package primes

import org.scalatest.{FlatSpec, Matchers}

class PrimesSpec extends FlatSpec with Matchers {

  "gcd" should "compute gcd" in {
    Primes.gcd(12, 8) shouldBe 4
  }

  "gcd" should "find coprimes" in {
    Primes.gcd(15, 14) shouldBe 1
  }

  "extended euclidien algo" should "find gcd and bezout coefs" in {

    // 1 = 240 * 23 + 46 * (-120)
    Primes.extendedGcd(240, 46) shouldBe (2, 23, -120)
  }

}
