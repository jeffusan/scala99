package advancedScala.classesAndInterfaces

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  val showInt:    Show[Int]    = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)

  def main(args: Array[String]) {
    val shownInt = 123.show
    val shownString = "abc".show
  }
}
