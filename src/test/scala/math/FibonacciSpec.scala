package math

import org.scalatest.{FlatSpec, Matchers}

class FibonacciSpec extends FlatSpec with Matchers {

  "fib" should "be correct for fist values" in {

    Fibonacci.fib(1) shouldBe 1
    Fibonacci.fib(2) shouldBe 1
    Fibonacci.fib(3) shouldBe 2
    Fibonacci.fib(4) shouldBe 3
    Fibonacci.fib(5) shouldBe 5
  }

  "binary exp" should "be correct" in {
    Fibonacci.binaryExponentiation(2, 2) shouldBe 4
    Fibonacci.binaryExponentiation(2, 3) shouldBe 8
    Fibonacci.binaryExponentiation(2, 5) shouldBe 32
  }

}
