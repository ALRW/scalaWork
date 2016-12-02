import org.scalatest.{FunSpec, Matchers}
class Ch3Spec extends FunSpec with Matchers {

  describe("pattern matching"){
    it("should return the correctly matched value"){
      assert(Ch3.x == 3)
    }
  }

  describe("#tail"){
    it("should return the tail of a list"){
      assert(List.tail(List(4,5,6)) == Cons(5, Cons(6, Nil)))
    }
    it("should return Nil if empty"){
      assert(List() == Nil)
    }
    it("should return Nil if list of one item"){
      assert(List.tail(List(1)) == Nil)
    }
  }

  describe("#setHead"){
    it("should replace the first element of a list with a given value"){
      assert(List.setHead(5, List(1,2)) == List(5,2))
    }
    it("should insert the first provided element if the list is empty"){
      assert(List.setHead(4, List()) == List(4))
    }
  }

  describe("#drop"){
    it("should drop the first n elements from a list"){
      assert(List.drop(List(1,2,3,4,5,6), 3) == List(4,5,6))
    }
  }

  describe("#dropWhile"){
    it("should drop elements from the list while they satisfy a condition"){
      assert(List.dropWhile(List(1,2,3,4,5,6,1), (x: Int) => x < 3) == List(3,4,5,6,1))
    }
    it("should return nil when supplied an empty list"){
      assert(List.dropWhile(List(), (x:Int) => x >= 4) == Nil)
    }
  }

  describe("#init"){
    it("drops the last element of a list and returns the initial items"){
      assert(List.init(List(1,2,3,4)) == List(1,2,3))
    }
    it("returns nil if passed and empty list"){
      assert(List.init(List()) == Nil)
    }
    it("returns an empty list if passed a list of one item"){
      assert(List.init(List(1)) == Nil)
    }
  }
}
