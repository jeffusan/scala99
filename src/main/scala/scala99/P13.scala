package scala99

object P13 {

  def main(args: Array[String]) {

    println(encodeDirect(List('a','b','c','a','b','c','a')))
  }

  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }

}
