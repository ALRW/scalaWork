import org.scalatest.{FunSpec, Matchers}

class StreamSpec extends FunSpec with Matchers {

  describe("#toList") {
    it("converts a stream to a list") {
      Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
    }
  }

  describe("#take") {
    it("takes the first n elements of a stream") {
      Stream(1, 2, 3, 4).take(2).toList shouldBe List(1, 2)
    }
  }

  describe("#drop") {
    it("drops the first n elements of a stream") {
      Stream(1, 2, 3, 4).drop(3).toList shouldBe List(4)
    }
  }

  describe("#takeWhile") {
    it("returns elements that match the given function") {
      Stream(1, 2, 3, 4).takeWhile(_ < 3).toList shouldBe List(1, 2)
    }
  }

  describe("#exists") {
    it("returns a boolean from running a function against a stream") {
      Stream(2, 3, 5).exists(_ == 1) shouldBe false
      Stream(1, 2, 3).exists(_ == 1) shouldBe true
    }
  }

  describe("#foldRight") {
    it("generalises a recursive function across a Stream") {
      Stream(1, 2, 3).foldRight(0)((a, x) => a + x) shouldBe 6
    }
  }

  describe("#foldExists") {
    it("uses foldRight to implement exists") {
      Stream(2, 3, 5).exists(_ == 1) shouldBe false
      Stream(1, 2, 3).exists(_ == 1) shouldBe true
    }
  }

  describe("#forall"){
    it("checks that all items of a Stream fulfil a given predicate"){
      Stream(1,2,4,5).forAll(_ > 2) shouldBe false
      Stream(1,2,3,4).forAll(_ > 0) shouldBe true
    }
  }

  describe("#foldTakeWhile"){
    it("use foldRight to implement takeWhile"){
      Stream(1, 2, 3, 4).foldTakeWhile(_ < 3).toList shouldBe List(1, 2)
    }
  }

  describe("#headOption"){
    it("uses foldRight to return an option of the Head element") {
      Stream(1, 2, 3, 4).headOption shouldBe Some(1)
      Stream.empty.headOption shouldBe None
    }
  }

  describe("#map"){
    it("applies a function to each element of a stream (implemented using foldRight)"){
      Stream(1,2,3).map(_ + 1).toList shouldBe List(2,3,4)
    }
  }

  describe("#append"){
    it("appends an item to a stream"){
      Stream(1,2,3).append(Stream(4)).toList shouldBe List(1,2,3,4)
    }
  }

  describe("#flatMap"){
    it("applies a function resulting in a Stream to each item in the Stream and returns a Stream only one level deep"){
      Stream(1,2,3).flatMap((x) => Stream(x)).toList shouldBe List(1,2,3)
    }
  }

  describe("#constant"){
    it("returns an infinite stream of a given value"){
      Stream.constant("a").take(5).toList shouldBe List("a","a","a","a","a")
    }
  }

  describe("#from"){
    it("generates an stream of infinitely increasing numbers"){
      Stream.from(3).take(5).toList shouldBe List(3,4,5,6,7)
    }
  }

  describe("#fibs"){
    it("generate an infinite stream of fibonacci numbers"){
      Stream.fibs.take(8).toList shouldBe List(0,1,1,2,3,5,8,13)
    }
  }

  describe("#unfold"){
    it("generalises the creation of infinite streams"){
      Stream.unfold(1)(_ => Some((1,1))).take(3).toList shouldBe List(1,1,1)
    }
  }

  describe("#unfoldOnes"){
    it("uses unfold to creat an infinite stream of 1's"){
      Stream.unfoldOnes.take(3).toList shouldBe List(1,1,1)
    }
  }

  describe("#unfoldConstant"){
    it("uses unfold to create an infinite stream of a constant n"){
      Stream.unfoldConstant(1).take(3).toList shouldBe List(1,1,1)
    }
  }

  describe("#unfoldFrom"){
    it("uses unfold to create an infinite stream of integers from a constant n onwards"){
      Stream.unfoldFrom(2).take(5).toList shouldBe List(2,3,4,5,6)
    }
  }

  describe("#unfoldfibs"){
    it("uses unfold to produce the infinite stream of fibonacci numbers"){
      Stream.unfoldfibs.take(8).toList shouldBe List(0,1,1,2,3,5,8,13)
    }
  }

  describe("#unfoldMap"){
    it("applies a function to each element of a stream (implemented using foldRight)"){
      Stream(1,2,3).unfoldMap(_ + 1).toList shouldBe List(2,3,4)
    }
  }

  describe("#unfoldTake") {
    it("takes the first n elements of a stream using unfold") {
      Stream(1, 2, 3, 4).unfoldTake(2).toList shouldBe List(1, 2)
    }
  }

  describe("#unfoldTakeWhile") {
    it("returns elements that match the given function using unfold") {
      Stream(1, 2, 3, 4).unfoldTakeWhile(_ < 3).toList shouldBe List(1, 2)
    }
  }

  describe("#zipWith"){
    it("combines two streams together with a function"){
      Stream(1,2).zipWith(Stream(2, 3))((a, b) => a + b).toList shouldBe List(3,5)
    }
  }

  describe("#zipAll"){
    it("combines two streams continuing even if one stream is exhausted"){
      Stream(1,2).zipAll(Stream(1,2,3)).toList shouldBe List((Some(1),Some(1)), (Some(2), Some(2)), (None, Some(3)))
    }
  }

  describe("#startsWith"){
    it("uses other functions to check whether one stream starts with another"){
      Stream(1,2,3).startsWith(Stream(1,2)) shouldBe true
    }
  }

  describe("#tails"){
    it("returns a stream of streams containing the suffixes contained in the initial stream"){
      Stream(1,2,3).tails.take(1).flatMap(x=>x).toList shouldBe List(1,2,3)
    }
  }

  describe("#hasSubsequence"){
    it("tests whether a stream contains a particular subsequence"){
      Stream(1,2,3,4,5).hasSubsequence(Stream(2,3,4)) shouldBe true
      Stream(4,5,6).hasSubsequence(Stream(1,2)) shouldBe false
    }
  }

  describe("#scanRigh"){
    it("generalises tails"){
      Stream(1,2).scanRight(0)(_+_).toList shouldBe List(3,2,0)
    }
  }

}
