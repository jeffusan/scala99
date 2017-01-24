package stack

import org.scalatest.{FlatSpec, Matchers}

class InfixToPostfixSpec extends FlatSpec with Matchers {

  val defaultCapacity = 20

  "a+b*c-d" should "abc*d-+" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("a+b*c-d")

    result.right.get shouldBe "abc*d-+"

  }

  "a+b*c+d" should "abc*d++" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("a+b*c+d")

    result.right.get shouldBe "abc*d++"
  }

  "a+b+c*d" should "abcd*++" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("a+b+c*d")

    result.right.get shouldBe "abcd*++"
  }

  "(a+b*c)*d-e" should "abc*+d*e-" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("(a+b*c)*d-e")

    result.right.get shouldBe "abc*+d*e-"
  }

  "a+b*(c^d-e)^(f+g*h)-i" should "abcd^e-fgh*+^*+i-" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("a+b*(c^d-e)^(f+g*h)-i")

    result.right.get shouldBe "abcd^e-fgh*+^*+i-"
  }

  "input too long" should "fail" in {
    val sut = new InfixToPostfix(defaultCapacity)
    val result = sut.transform("123456789123456789132456789")

    result.left.get shouldBe InfixToPostfixFailure(InfixToPostfix.tooLong + defaultCapacity)
  }

}
