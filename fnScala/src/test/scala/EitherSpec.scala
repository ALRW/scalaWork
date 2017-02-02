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
}
