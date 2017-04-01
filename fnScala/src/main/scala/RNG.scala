trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rn) = rng.nextInt
    val (i2, nrn) = rn.nextInt
    ((i1,i2), nrn)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nrs) = nextInt
    (if(i < 0) -(i + 1) else i, nrs)
  }
}
