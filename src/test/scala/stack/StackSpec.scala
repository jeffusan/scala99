package stack

import org.scalatest.{FlatSpec, Matchers}

class StackSpec extends FlatSpec with Matchers {

  val capacity = 10

    "A Stack" should "pop values in last-in-first-out order" in {
      val stack = new Stack(capacity)
      stack.push(1)
      stack.push(2)
      stack.pop() should be (Right(2))
      stack.pop() should be (Right(1))
    }

    it should "return a Failure if poping an empty stack" in {
      val emptyStack = new Stack(capacity)
      emptyStack.pop() should be (Left(Failure(Failure.underflow)))
    }

  "An array" should "return the element at a given index" in {
    val array = new Array[Int](capacity)
    array(0) = 1
    array(0) shouldBe 1
  }

  "A stack" should "peek the top of the stack" in {
    val stack = new Stack(capacity)

    stack.push(1)
    stack.peek() should be (Right(1))

    stack.push(2)
    stack.peek() should be (Right(2))

    stack.pop()
    stack.peek() should be (Right(1))
  }

  "Peeking at an empty stack" should "return a Failure" in {
    val stack = new Stack(capacity)
    stack.peek() should be (Left(Failure(Failure.empty)))
  }

}
