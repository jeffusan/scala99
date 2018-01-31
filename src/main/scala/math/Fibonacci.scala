package math

object Fibonacci {

  def fib(n: Int): Int = {
    if(n == 1 || n == 2) 1
    else {
      val (result, _) = (1 to n - 2).foldLeft((1, 1)) {
        case ((last, prevLast), _) ⇒
          val r = last + prevLast
          (r, last)
      }
      result
    }
  }

  /**
    * O(log(n))
    */
  def binaryExponentiation(x: Double, n: Int): Double = {
    if(n == 0) 1
    else if(n == 1) x
    else {
      val t = binaryExponentiation(x, n / 2)
      if(n % 2 == 0)
        t * t
      else
        x * t * t
    }
  }

}
