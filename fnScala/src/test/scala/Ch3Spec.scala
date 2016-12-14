import org.scalatest.{FunSpec, Matchers}

class Ch3Spec extends FunSpec with Matchers {

  describe("#pattern matching"){
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

  describe("#foldLeft"){
    it("tail recursively applies a function to all elements of a list"){
      assert(List.foldLeft(List(1,2,3,4,5), 4)((a,b) => a + b) == 19)
    }
  }

  describe("#length"){
    it("computes the length of a List using foldRigth"){
      assert(List.length(List(1,2,3,4,5)) == 5)
    }
  }

  describe("#foldSum"){
    it("uses foldLeft to compute the sum of a list"){
      assert(List.foldSum(List(1,2,3,4,5)) == 15)
    }
  }

  describe("#foldProduct"){
    it("uses foldLeft to compute the product of a list"){
      assert(List.foldProduct(List(2,2,2,2)) == 16)
    }
  }

  describe("#foldLength"){
    it("uses foldLeft to compute the length of a list"){
      assert(List.foldLength(List(1,1,1,1,1,1,1,1)) == 8)
    }
  }

  describe("#foldAppend"){
    it("uses a fold method to append two lists together"){
      assert(List.foldAppend(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
    }
  }

  describe("#addOne"){
    it("adds one to each element of a list of integers"){
      assert(List.addOne(List(1,2,3,4)) == List(2,3,4,5))
    }
  }

  describe("#doubleToString"){
    it("converts each element of a list of doubles to strings"){
      assert(List.doubleToString(List(1.1,1.4,1.5)) == List("1.1", "1.4", "1.5"))
    }
  }

  describe("#map"){
    it("modifies each element of a list according to a given function"){
      assert(List.map(List(1,2,3,4,5))((e) => e*2) == List(2,4,6,8,10))
    }
  }

  describe("#filter"){
    it("remove all elements from a list unless they satisfy a given predicate"){
      assert(List.filter(List(1,2,3,4,5))((e) => e%2 == 0) == List(2,4))
    }
  }

  describe("#flatMap"){
    it("applies a function on each element and returns a single list"){
      assert(List.flatMap(List(1,2,3)) (i=>List(i,i)) == List(1,1,2,2,3,3))
    }
  }

  describe("#flatMapFilter"){
    it("uses flatMap to implement filter"){
      assert(List.flatMapFilter(List(2,3,4,5))(e => e%2 != 0) == List(3,5))
    }
  }

  describe("#zipper"){
    it("takes two lists of Ints and creates a new list of the sums of Ints"){
      assert(List.zipper(List(1,2,3), List(1,2,3)) == List(2,4,6))
    }
  }

}
