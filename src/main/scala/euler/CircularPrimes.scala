package euler

object CircularPrimes {

  /**
    * The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
    * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
    * How many circular primes are there below one million?
    */
  def circularPrimesCount(n: Int): Int = {
    (0 to n).foldLeft(0) {
      (c, i) ⇒ if (isPrime(i)) c + 1 else c
    }
  }

  def isPrime(n: Long): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else
      !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n % _ == 0))
  }

  def main(args: Array[String]): Unit = {
    println(isPrime(2))
  }

}
