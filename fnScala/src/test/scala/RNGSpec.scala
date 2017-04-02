import org.scalatest.{FunSpec, Matchers}

class RNGSpec extends FunSpec with Matchers {

  val rng = SimpleRNG(1)
  describe("#nonNegativeInt") {
    it("should generate a positive random number") {

      val randomInt = rng.nonNegativeInt(rng)._1
      randomInt should be < Int.MaxValue
      randomInt should be > 0
    }
  }

  describe("#double") {
    it("returns a random double between 0 and 1 exlclusive") {

      val randomDouble = rng.double(rng)._1
      randomDouble should be < 1.0
      randomDouble should be >= 0.0
    }
  }

  describe("#intDouble") {
    it("provides an Int Double pair") {
      val randomid = rng.intDouble(rng)._1
      randomid should matchPattern { case Tuple2(_: Int, _: Double) => }
    }
  }

  describe("#doubleInt") {
    it("provides a Double Int pair") {
      val (x, y) = rng.doubleInt(rng)._1
      x should matchPattern { case _: Double => }
      y should matchPattern { case _: Int => }
    }
  }

  describe("#double3") {
    it("provides a tuple3 of Doubles") {
      val rd3 = rng.double3(rng)._1
      rd3 should matchPattern { case Tuple3(_: Double, _: Double, _: Double) => }
    }
  }

  describe("#ints") {
    it("generate a list of random integers") {
      val (l, nrng) = rng.ints(2)(rng)
      List.length(l) shouldBe 2
    }
  }

}
