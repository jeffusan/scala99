package datastructure.greedy

object Greedy {

  /**
    * assumes l is sorted in decreasing order
    */
  def change(l: List[Int], a: Int): List[Int] = {
    def go(denom: List[Int], amount: Int, acc: List[Int]): List[List[Int]] = {
      if(amount == 0) List(acc)
      else {
        denom match {
          case Nil => List() //amount not 0 and no denom left: not working
          case x :: xs if amount < 0 => List()
          case x :: xs => go(denom, amount - x, x :: acc) ++ go(xs, amount, acc)
        }
      }
    }
    go(l, a, List()).head
  }

  def changeJava(l: List[Int], a: Int): List[Int] = {
    changeJavaHelper(l, a, List()).head
  }

  private def changeJavaHelper(denom: List[Int], amount: Int, acc: List[Int]): List[List[Int]] = {
    if(amount == 0) List(acc)
    else {
      if(denom.isEmpty) List()
      else if(amount < 0) List()
      else {
        changeJavaHelper(denom, amount - denom.head, denom.head :: acc) ++ changeJavaHelper(denom.tail, amount, acc)
      }
    }
  }

}
