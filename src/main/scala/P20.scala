object P20 {

  def main(args: Array[String]) {
    val list = List('a, 'b, 'c, 'd)
    println(list)
    println(removeAtJ2(1, list))
  }

  def removeAt(i:Int, list: List[Symbol]): (List[Symbol], Symbol) = {

    val c = list(i)
    val res = list.zipWithIndex.filter { x =>  x._2 != i } map { _._1}
    (res, c)
  }

  def removeAtInt(i:Int, list: List[Int]): (List[Int], Int) = {
    val c = list(i)
    val res = list.zipWithIndex.filter { x =>  x._2 != i } map { _._1}
    (res, c)
  }

  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt2(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }

  def removeAtJ[A](n: Int, ls: List[A]): (List[A], A) = {
    var result = List[A]()
    for (i <- ls.length -1 to 0 by -1 ) {
      if(i != n) result = ls(i) :: result
    }
    (result, ls(n))
  }

  def removeAtJ2[A](n: Int, ls: List[A]): (List[A], A) = {
    if(n < 0 || ls.length == 0) throw new NoSuchElementException
    if(n == 0 ) (ls.tail, ls.head)
    else {
      val (l, e) = removeAtJ2(n-1, ls.tail)
      (ls.head :: l, e)
    }
  }
}
