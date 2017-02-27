object P21 {

  def main(args: Array[String]) {

    val ls = List('a, 'b, 'c, 'd)

    println(insertAt('new, 0, ls))
    println(insertAt('new, 1, ls))
    println(insertAt('new, 2, ls))
    println(insertAt('new, 3, ls))
    println(insertAt('new, 4, ls))
  }

  def insertAt(s: Symbol,i: Int, ls: List[Symbol]): List[Symbol] = ls.splitAt(i) match {
    case x => x._1 ::: s :: x._2
  }

  def insertAt2(s: Symbol,i: Int, ls: List[Symbol]): List[Symbol] = {
    if(i < 0 || i >= ls.length) throw new Exception
    val l = ls.length
    (i, ls) match {
      case (_, Nil) => throw new Exception
      case (0, list) => s::list
//      case (l , list) => list:::List(s)
      case (n, h::tail) => h::insertAt(s, i -1, tail)
    }
  }

}
