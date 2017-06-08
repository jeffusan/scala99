package scala99

object P04 {
  def main(args: Array[String]) {
    println(length(0, List(1,2,3)))
  }

  /**
   * find the number of element in the list
   */
  def length(count: Int, list: List[Int]): Int = (count, list) match {
    case (c, Nil) => c
    case (c , _ :: tail) => length(c + 1 , tail)
  }
}
