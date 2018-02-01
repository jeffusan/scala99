package datastructure.advancedLists

/**
  * We use a List[Int] to represent binary numbers, a list of 0's and 1's.
  * If you pass in a list that has any other numbers except 0 or 1, the algorithms will throw an exception.
  *
  * We write a binary number from left to right.
  * In other words, the most significant bit is at the leftmost
  * and the least significant bit of a binary number is at the rightmost.
  *
  */
object Binary {

  def carry(c: Int, list: List[Int]): List[Int] = (c, list) match {
    case (0, xs) => xs
    case (1, Nil) => List(1)
    case (1, x::xs) => (1 - x) :: carry(x, xs)
    case (_, _) => sys.error("bad input")
  }

  def add(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (xs, Nil) => xs
    case (Nil, xs) => xs
    case (e::es, f::fs) => ((e + f) % 2) :: carry((e + f) / 2, add(es, fs))
//    case (e::es, f::fs) if e == 0 || f == 0 => (e + f) :: add(es, fs)
//    case (e::es, f::fs) if e == 1 && f == 1 => 0 :: carry(1, add(es, fs))
    case _ => sys.error("invalid input")
  }

  /**
    * assumes bits in reverse order: most significant bit at the tail
    */
  def multiply(a: List[Int], b: List[Int]): List[Int] = b match {
    case Nil => Nil
    case 0 :: xs => 0 :: multiply(a, xs) // assumes there will be a 1 at some point later => left shift operation (multiply by 2)
    case 1 :: xs => add(a, 0 :: multiply(a, xs)) //add itself (a) to final result: the least significant bit will be 0 because the bit is the same
    case _ => sys.error("bad input")
  }


}
