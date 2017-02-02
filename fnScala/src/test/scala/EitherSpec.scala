import org.scalatest.{FunSpec, Matchers}


class EitherSpec extends FunSpec with Matchers {

  describe("map"){
    it("perfoms a function f on the value of a right or returns a left if it fails"){
      Right(1).map(_ + 2) should be(Right(3))
    }
    it("should return the Left if asked to operate on a Left"){
      Left("Hello").map(a  => a.##) should be (Left("Hello"))
    }
  }

  describe("flatMap"){
    it("runs a potentially failing funciton on the value held in an Either"){
      Right(1).flatMap(a => Right(a)) should be (Right(1))
    }
    it("returns a Left if the function is run on a left"){
      Left(1).flatMap(a => Right(a)) should be(Left(1))
    }
  }

  describe("orElse"){
    it("returns a given default when passed a left"){
      Left(1).orElse(Right("failing value")) should be(Right("failing value"))
    }
    it("returns a given value when correct"){
      Right(1).orElse(Right("failing value")) should be(Right(1))
    }
  }

  describe("map2"){
    it("takes two potentially failing operations and if they pass performs a potentially failing function f on them"){
      Right(1).map2(Right(2))((a, b) => a + b) should be(Right(3))
    }
  }
}
