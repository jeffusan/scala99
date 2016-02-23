object P26 {

  def main(args: Array[String]) {

    val result = combinations2(3, List('a, 'b, 'c, 'd, 'e, 'f))
    println(result)
    println(result.length)
//    println(combinations(2, List('a, 'b, 'c)))

//    println(combinations(5, List('a, 'b, 'c)))


  }


  def combinations2(x: Int, list: List[Symbol]): List[List[Symbol]] = {

    println(" x: " + x + " list: " + list)

    if (list.length <= 0 || x <= 0)
      List()

     else {
      val head = list.head
      val rest = list.tail
      val tmp: List[List[Symbol]] = combinations(x -1, rest)
      val result = tmp.map ( comb => head::comb)
      result ::: combinations(x, rest)
    }
  }

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {

    println(" x: " + n + " list: " + ls)
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {
        sl.head :: _
      }
    }
  }


// case @:

//  It enables one to bind a matched pattern to a variable. Consider the following, for instance:
//
//  val o: Option[Int] = Some(2)
//  You can easily extract the content:
//
//    o match {
//    case Some(x) => println(x)
//    case None =>
//  }
//  But what if you wanted not the content of Some, but the option itself? That would be accomplished with this:
//
//    o match {
//    case x @ Some(_) => println(x)
//    case None =>
//  }



}
