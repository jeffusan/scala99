object P17 {

  def main(args: Array[String]) {
    println(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
    println(split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

  }

  def split(x: Int, list: List[Char]): (List[Char], List[Char]) = {
    list.splitAt(x)

    (list.take(x), list.drop(x))

  }


}
