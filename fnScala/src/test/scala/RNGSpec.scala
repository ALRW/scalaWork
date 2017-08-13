import RNG.SimpleRNG
import org.scalatest.{FunSpec, Matchers}

class RNGSpec extends FunSpec with Matchers {


  describe("#nonNegativeInt") {
    it("should generate a positive random number") {
      val randomInt = RNG.nonNegativeInt(SimpleRNG(1))._1
      randomInt should be < Int.MaxValue
      randomInt should be > 0
    }
  }

  describe("#double") {
    it("returns a random double between 0 and 1 exlclusive") {
      val (a, _) = RNG.double(SimpleRNG(3))
      a shouldBe 5.374876782298088E-4
    }
  }

  describe("#intDouble") {
    it("provides an Int Double pair") {
      val (a, _) = RNG.intDouble(SimpleRNG(3))
      a should matchPattern { case Tuple2(_: Int, _: Double) => }
    }
  }

  describe("#doubleInt") {
    it("provides a Double Int pair") {
      val (x, y) = RNG.doubleInt(SimpleRNG(2))._1
      x should matchPattern { case _: Double => }
      y should matchPattern { case _: Int => }
    }
  }

  describe("#double3") {
    it("provides a tuple3 of Doubles") {
      val (a, _) = RNG.double3(SimpleRNG(2))
      a should matchPattern { case Tuple3(_: Double, _: Double, _: Double) => }
    }
  }

  describe("#ints") {
    it("generate a list of random integers") {
      val (l, _) = RNG.ints(2)(SimpleRNG(5))
      List.length(l) shouldBe 2
    }
  }

  describe("#mapDouble") {
    it("returns a random double between 0 and 1 exclusive using map for its implementation") {
      val (a, _) = RNG.mapDouble(SimpleRNG(42))
      a shouldBe 0.007524831686168909
    }
  }

  describe("#map2") {
    it("takes two actions and apply's a function to combine their results and returns a new action from the combination") {
      val (a, _) = RNG.map2(RNG.int, RNG.mapDouble)((_,_))(SimpleRNG(22))
      a shouldBe (8464475,0.16455321665853262)
    }
  }



}
