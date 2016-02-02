object P02 {

  def main(args: Array[String]) {


    println(almostLastRecur(List(1,2,3,5)))
  }

  def almostLast(list: List[Int]): Int = {
//    list(list.length - 2)
    list.init.last

  }

  def almostLastRecur(list: List[Int]): Int = list match {

    case h :: _ :: Nil => h
    case _ :: tail => almostLastRecur(list.tail)
    case _  => throw new NoSuchElementException

  }

}
