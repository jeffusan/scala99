object P12 {

  def main(args: Array[String]) {
    println(decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'), 'z')))
    println(decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))))
  }

  def decode(list: List[Any]): List[Char] = list.foldRight(List[Char]()) {
    (element: Any, result: List[Char]) => {
      element match {

        case elt: Char => elt.toChar :: result

        case tuple: (Int, Char) => {
          var tmp = result
          for(i <- 0 until tuple._1){
            tmp = tuple._2 :: tmp
          }
          tmp
        }
      }
    }
  }

  def decode2(list: List[(Int, Char)]): List[Char] = {
    list flatMap { e: (Int, Char) => List.fill(e._1){e._2} }
  }

//  def decode3[A](ls: List[(Int, A)]): List[A] =
//    ls flatMap { e => List.make(e._1, e._2) }

}
