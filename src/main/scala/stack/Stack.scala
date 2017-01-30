package stack

import scala.reflect.ClassTag

class Stack[T](protected val elements: Array[T]) {

  import Failure._

  val capacity: Int = elements.length

  protected var top: Int = -1

  /**
    * O(1)
    */
  def push(e: T): Either[Failure, Success] = {
    if(top >= capacity ) Left(Failure(overflow))
    else {
      top+=1
      elements(top) = e
//      array.foreach(i => print(i + ","))
//      println("")
      Right(new Success)
    }
  }

  def pop(): Either[Failure, T]= {
    if(top < 0) Left(Failure(underflow))
    else {
//      println("poping at " + top + ": " + array(top))
      val result = elements(top)
      top -= 1
      Right(result)
    }
  }

  def peek(): Either[Failure, T] = {
    if(top < 0) Left(Failure(empty))
    else {
      Right(elements(top))
    }
  }

}

object Stack {

  /**
  *
  * A `ClassTag[T]` stores the erased class of a given type `T`, accessible via the `runtimeClass`
  * field. This is particularly useful for instantiating `Array`s whose element types are unknown
  * at compile time.
  */
  def printStack[T: ClassTag](stack: Stack[T]): Stack[T] = {

    if(stack.top> 0) {

      val copyToPrint = new Stack[T](new Array[T](stack.capacity))
      val copyToReturn = new Stack[T](new Array[T](stack.capacity))

      val tmp = new Array[T](stack.capacity)
      var i = 0
      while(stack.peek().isRight) {
        tmp(i) = stack.pop().right.get
        i += 1
      }

      val reversed = tmp.reverse
      reversed.foreach { e =>
        copyToPrint.push(e)
        copyToReturn.push(e)
      }

      val builder = new StringBuilder()
      while(copyToPrint.peek().isRight) {
        builder.append(copyToPrint.pop().right.get)
      }
      println(builder.reverse.toString())

      copyToReturn


    } else {
      println("empty")
      stack
    }



  }
}

class Success

case class Failure(reason: String)

object Failure {
  val overflow = "overflow"
  val underflow = "underflow"
  val empty = "empty stack"
}
