import org.scalatest.{FunSpec, Matchers}

class StreamSpec extends FunSpec with Matchers {

  describe("#toList"){
    it("converts a stream to a list"){
      Stream(1,2,3,4).toList should be(List(1,2,3,4))
    }
  }

  describe("#take"){
    it("takes the first n elements of a stream"){
      Stream(1,2,3,4).take(2).toList shouldBe List(1,2)
    }
  }

  describe("#drop"){
    it("drops the first n elements of a stream"){
      Stream(1,2,3,4).drop(3).toList shouldBe List(4)
    }
  }

  describe("#takeWhile"){
    it("returns elements that match the given function"){
      Stream(1,2,3,4).takeWhile( _ < 3).toList shouldBe List(1,2)
    }
  }

}
