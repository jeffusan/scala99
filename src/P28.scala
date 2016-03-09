object P28 {

//  (**) Sorting a list of lists according to length of sublists.
//    a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
//    Example:
//
//    scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//  res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
//  b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
//
//  Example:
//
//    scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//  res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//  Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
  def main(args: Array[String]) {

  println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
//  println(5 /2)
  }

  def lsort[A](list: List[List[A]]): List[List[A]] = {
    val length = list.length

    if(length <= 1) list
    else {

      val ls = list.toArray
      val l1 = length / 2
      val l2 = length - l1

      val a1: Array[List[A]] = new Array(l1)
      val a2: Array[List[A]] = new Array(l2)

      for (i <- 0 to l1 - 1) {
        a1.update(i, ls(i))
        a2.update(i, ls(l1 + i))
      }

      if(l2 % 2 == 0)
        a2.update(l2 -1, ls(length -1))

      val s1 = lsort(a1.toList)
      val s2 = lsort(a2.toList)

      val result: Array[List[A]] = new Array(length)

      var i1 = 0
      var i2 = 0

      for (i <- 0 to length - 1) {
        if (i2 >= l2 || i1 < l1 && s1(i1).length <= s2(i2).length) {
          result.update(i, s1(i1))
          i1 += 1
        } else {
          result.update(i, s2(i2))
          i2 += 1
        }
      }
      result.toList

    }
  }

}
