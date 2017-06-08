package scala99

object P03 {

  def main(args: Array[String]) {


    println(nth(2, List(1,2,3,5)))
  }

  /**
   *Find the nth element of a list
   */
  def nth(n: Int, list: List[Int]): Int = (n, list) match{
    case (0, h :: _) => h
    case (h, _:: tail) => nth(h-1, tail)
    case (_,Nil) => throw new NoSuchElementException
  }

}
