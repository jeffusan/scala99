object P14 {

  def main(args: Array[String]) {

    println(duplicate(List('a','b','c')))
  }

  def duplicate(list: List[Char]): List[Char] = {
    list flatMap {
      c => List(c, c)
    }
  }

}
