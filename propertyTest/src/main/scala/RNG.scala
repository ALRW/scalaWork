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

  type Rand[+A] = RNG => (A, RNG)

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rn) = rng.nextInt
    val (i2, nrn) = rn.nextInt
    ((i1, i2), nrn)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nrs) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nrs)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nrs) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), nrs)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, nrng) = rng.nextInt
    val (y, nnrng) = double(nrng)
    ((x, y), nnrng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((x, y), nrng) = intDouble(rng)
    ((y, x), nrng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (x, nrng) = double(rng)
    val (y, nnrng) = double(nrng)
    val (z, fnrng) = double(nnrng)
    ((x, y, z), fnrng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (x, nrng) = rng.nextInt
      val (xs, nnrng) = ints(count - 1)(nrng)
      (x :: xs, nnrng)
    }
  }

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def mapDouble: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r) = ra(rng)
      val (b, rr) = rb(r)
      (f(a, b), rr)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)((c, d) => c :: d))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def flatReMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(r => unit(f(r)))

  def flatReMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

