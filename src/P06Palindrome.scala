object P06Palindrome {

  def main(args: Array[String]) {
    println(palindrome(List(1,2,2,1)))
  }

  def palindrome(list: List[Int]): Boolean = list == list.reverse

}
