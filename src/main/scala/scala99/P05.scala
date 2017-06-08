package scala99

object P05 {

  def main(args: Array[String]) {


    println(reverseTail(List(1,2,3)))
  }


  /**
   * O(n*n)
   */
  def reverse(list: List[Int]): List[Int] = list  match {
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)
  }

  /**
   * tail recursive
   */
  def reverseTail(list: List[Int]): List[Int] = {
    def reverseMe(result: List[Int], cur: List[Int]): List[Int] = cur match {
      case Nil => result
      case h::tail => reverseMe(h :: result, tail)
    }
    reverseMe(List(), list)
  }


  def reverseFunc(list: List[Int]): List[Int] = {
    list.foldLeft(List[Int]()) { (h,t) => t :: h }
  }

}
