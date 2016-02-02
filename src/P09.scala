object P09 {

  def main(args: Array[String]) {
//    println(List('a','b','c','a','c','b'))

//    var q = List('a','b')
//    q = 'a' :: q
//    println(q)

//
//    val list = List(1, 2, 3, 4)
////    list: List[Int] = List(1, 2, 3, 4)
//
//    println(list.patch(2, Seq(5), 1)) // replaces one element of the initial sequence
////    res0: List[Int] = List(1, 2, 5, 4)
//
//println(list.patch(2, Seq(5), 2)) // replaces two elements of the initial sequence
////    res1: List[Int] = List(1, 2, 5)
//
//    println(list.patch(2, Seq(5), 0)) // adds a new element
//    println(list)



    println(concat(List('a','b','c','a','c','b')))
    println(pack(List('a','b','c','a','c','b')))
    println(packTail(List('a','b','c','a','c','b')))
  }

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls partition { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def packTail[A](ls: List[A]): List[List[A]] = {

    def packk(list: List[A], res: List[List[A]]): List[List[A]] = {
      if(list.isEmpty) res
      else{
        val (packed, next) = list partition { _ == list.head}
        if(next == Nil) packed :: res
        else {
          val resTmp = packed :: res
          packk(next, resTmp)
        }
      }
    }
    packk(ls, List())
  }

  def concat(list: List[Char]): List[List[Char]] = {

    list.foldLeft(List[List[Char]]()) { (resultList, element ) => {

//      val maybeChars: Option[List[Char]] = resultList.find(subList => subList.contains(element))
      val i: Int = resultList.indexWhere(subList => subList.contains(element))

      i match {
        case -1 => {
          val tmp = List(element)
          tmp :: resultList
        }
        case _ => {
          val tmp = element :: resultList(i)
          resultList.updated[List[Char], List[List[Char]]](i, tmp) //  patch[List[Char], List[List[Char]]](i, Seq(aa), 1)
        }

      }

//      val tmp = maybeChars.getOrElse({List(element)})
//
//      element :: tmp
//      println(tmp)
//      tmp :: resultList
//      b :: resultList


//
//      maybeChars match {
//        case Some(chars) =>  {
////          println("START")
////          println(chars)
////          println(chars)
////          println("END")
//
//          resultList.
//          (element :: chars) :: resultList
//
//        }
//
//        case None =>   {
//          val tmp = List(element)
//          tmp :: resultList
//
//
//        }
//      }

//      val a = List(element)
//      a :: resultList
    }

    }
  }

}
