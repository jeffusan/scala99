package fp.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * Int.Minvalue is 1 smaller than -(Int.MaxValue),
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, r) = rng.nextInt
    if (a == Int.MinValue) nonNegativeInt(r)
    else if (a < 0) (-a, r)
    else (a, r)
  }

  /**
    * double between 0 and 1
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d = (i/Int.MaxValue).toDouble
    (d,r)
  }

}