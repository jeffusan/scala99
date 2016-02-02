object P18 {

  def main(args: Array[String]) {
    println(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
    println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

    println("123".toLong)


  }

  def slice(i: Int,j: Int, list: List[Symbol]): List[Symbol] = {
    list drop(i) take(j- i)

//    list drop i take (j - (i max 0))

  }

}
