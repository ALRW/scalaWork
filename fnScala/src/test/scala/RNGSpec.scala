import org.scalatest.{FunSpec, Matchers}

class RNGSpec extends FunSpec with Matchers{

  describe("#nonNegativeInt"){
    it("should generate a positive random number"){
      val rng = SimpleRNG(1)

      val randomInt = rng.nonNegativeInt(rng)._1
      randomInt should be < Int.MaxValue
      randomInt should be > 0
      (rng.nonNegativeInt(rng)._1 > 0) shouldBe true
    }
  }

}
