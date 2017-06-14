package advancedScala.monoids

import org.scalatest.{FlatSpec, Matchers}

class MonoidsSpec extends FlatSpec with Matchers {

  "boolean AND monoid" should "pass associative Law all true" in {
    Monoid.associativeLaw(true, true, true)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 2" in {
    Monoid.associativeLaw(false, true, true)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 3" in {
    Monoid.associativeLaw(true, false, true)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 4" in {
    Monoid.associativeLaw(true, true, false)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 5" in {
    Monoid.associativeLaw(false, false, true)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 6" in {
    Monoid.associativeLaw(true, false, false)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 7" in {
    Monoid.associativeLaw(false, false, false)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass associative Law 8" in {
    Monoid.associativeLaw(false, false, false)(Monoid.andMonoid) shouldBe true
  }

  "boolean AND monoid" should "pass identity Law" in {
    Monoid.identityLaw(false)(Monoid.andMonoid) shouldBe true
    Monoid.identityLaw(true)(Monoid.andMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 1" in {
    Monoid.associativeLaw(false, false, false)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 2" in {
    Monoid.associativeLaw(false, true, true)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 3" in {
    Monoid.associativeLaw(true, false, true)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 4" in {
    Monoid.associativeLaw(true, true, false)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 5" in {
    Monoid.associativeLaw(false, false, true)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 6" in {
    Monoid.associativeLaw(true, false, false)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 7" in {
    Monoid.associativeLaw(false, false, false)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass associative Law 8" in {
    Monoid.associativeLaw(true, true, true)(Monoid.orMonoid) shouldBe true
  }

  "boolean OR monoid" should "pass identity Law" in {
    Monoid.identityLaw(false)(Monoid.orMonoid) shouldBe true
    Monoid.identityLaw(true)(Monoid.orMonoid) shouldBe true
  }
}
