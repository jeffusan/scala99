package stack

import scala.reflect.ClassTag

class Stack[T: ClassTag](val capacity: Int) {

  import Failure._

  protected val array = new Array[T](capacity)

  protected var top: Int = -1

  /**
    * O(1)
    */
  def push(e: T): Either[Failure, Success] = {
    if(top >= capacity ) Left(Failure(overflow))
    else {
      top+=1
      array(top) = e
//      array.foreach(i => print(i + ","))
//      println("")
      Right(new Success)
    }
  }

  def pop(): Either[Failure, T]= {
    if(top < 0) Left(Failure(underflow))
    else {
//      println("poping at " + top + ": " + array(top))
      val result = array(top)
      top -= 1
      Right(result)
    }
  }

  def peek(): Either[Failure, T] = {
    if(top < 0) Left(Failure(empty))
    else {
      Right(array(top))
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
