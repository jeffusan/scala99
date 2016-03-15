package arithmetic

class S99Int(val start: Int) {

  def isPrime: Boolean = {
    if (start < 2) false
    else if (start == 2 ) true
    else if (start % 2 == 0) false
    else {
      var result = true

      val sqrt: Double = Math.sqrt(start)

      for (i <- 3 to sqrt.toInt by 2) {
        if (start % i == 0) {
          println("divisible by " + i)
          result = false
        }
      }
      result
    }
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def main(args: Array[String]) {
    println(234234239.isPrime)

  }
}