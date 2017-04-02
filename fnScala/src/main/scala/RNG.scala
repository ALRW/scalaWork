trait RNG {
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {

  type Rand[+A] = RNG => (A, RNG)

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

  def double(rng: RNG): (Double, RNG) = {
    val (i, nrs) = nonNegativeInt(rng)
    (i / (Int.MaxValue + 1), nrs)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, nrng) = rng.nextInt
    val (y, nnrng) = double(nrng)
    ((x, y), nnrng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((x, y), nrng) = intDouble(rng)
    ((y,x), nrng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (x, nrng) = double(rng)
    val (y, nnrng) = double(nrng)
    val (z, fnrng) = double(nnrng)
    ((x, y, z), fnrng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) ={
    if(count == 0) (List(), rng)
    else {
      val (x, nrng) = rng.nextInt
      val (xs, nnrng) = ints(count - 1)(nrng)
      (Cons(x, xs), nnrng)
      }
  }
}
