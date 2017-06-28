package advancedScala.monoids

import org.scalatest.{FlatSpec, Matchers}

class MonoidsSpec extends FlatSpec with Matchers {

  "boolean AND monoid" should "pass associative Law all true" in {
    Monoids.associativeLaw(true, true, true)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 2" in {
    Monoids.associativeLaw(false, true, true)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 3" in {
    Monoids.associativeLaw(true, false, true)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 4" in {
    Monoids.associativeLaw(true, true, false)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 5" in {
    Monoids.associativeLaw(false, false, true)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 6" in {
    Monoids.associativeLaw(true, false, false)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 7" in {
    Monoids.associativeLaw(false, false, false)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 8" in {
    Monoids.associativeLaw(false, false, false)(Monoids.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass identity Law" in {
    Monoids.identityLaw(false)(Monoids.andMonoid) shouldBe true
    Monoids.identityLaw(true)(Monoids.andMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 1" in {
    Monoids.associativeLaw(false, false, false)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 2" in {
    Monoids.associativeLaw(false, true, true)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 3" in {
    Monoids.associativeLaw(true, false, true)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 4" in {
    Monoids.associativeLaw(true, true, false)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 5" in {
    Monoids.associativeLaw(false, false, true)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 6" in {
    Monoids.associativeLaw(true, false, false)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 7" in {
    Monoids.associativeLaw(false, false, false)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 8" in {
    Monoids.associativeLaw(true, true, true)(Monoids.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass identity Law" in {
    Monoids.identityLaw(false)(Monoids.orMonoid) shouldBe true
    Monoids.identityLaw(true)(Monoids.orMonoid) shouldBe true
  }
}
