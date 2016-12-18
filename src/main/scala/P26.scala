object P26 {

  def main(args: Array[String]) {

    val result = combinations2(4, List('a, 'b, 'c, 'd, 'e, 'f))
    println(result)
    println(result.length)
//    println(combinations(2, List('a, 'b, 'c)))

//    println(combinations(5, List('a, 'b, 'c)))


  }


  def combinations2[A](x: Int, list: List[A]): List[List[A]] = {

//    println(" x: " + x + " list: " + list)

    if(list == Nil) Nil
    else if (x == 0) List(Nil)
    else if (x == 1 && list.length ==1) List(list)

    else {
      val head = list.head
      val rest = list.tail
      val tmp: List[List[A]] = combinations2(x - 1, rest)
//      println("tmp: " + tmp)

      val result: List[List[A]] = tmp.map(comb => head :: comb)
//      println("result: " + result)


      val restComb = combinations2(x, rest)
      val r = result ::: restComb
//      println("final: " + r + " = " + result + " + " + restComb)

      r

    }
  }

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {

//    println(" x: " + n + " list: " + ls)
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
