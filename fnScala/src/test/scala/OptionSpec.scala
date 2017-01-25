import org.scalatest.{FunSpec, Matchers}

class OptionSpec extends FunSpec with Matchers{

  describe("#map"){
    it("applies a function to the contents of the option if it exists"){
      assert(Some(1).map(_ + 1) == Some(2))
    }
  }

  describe("#flatMap"){
    it("applies a function that might fail to a argument that might be there"){
      assert(Some(2).flatMap(a => Some(a + 1)) == Some(3))
    }
    it("return None if the function fails"){
      assert(Some(2).flatMap(a => None) == None)
    }
  }

  describe("#getOrElse"){
    it("returns the value of an option or returns the default"){
      assert(None.getOrElse("the default") == "the default")
    }
    it("returns the value of an option when given one"){
      assert(Some("hello").getOrElse("the default") == "hello")
    }
  }

  describe("#orElse"){
    it("provides a default value for an option if None exists"){
      assert(None.orElse(Some("the default")) == Some("the default"))
    }
  }

  describe("#filter"){
    it("filters the options results"){
      assert(Some(1).filter(_ % 2 == 0) == None)
    }
    it("returns the option of the result if they pass the filter"){
      assert(Some(2).filter(_ % 2 == 0) == Some(2))
    }
  }

  describe("#variance"){
    it("calculates the possible variance of a list using flatmap"){
      assert(Option.variance(Seq(1.1,1.2,1.2,1.3)) == Some(0.0049999999999999975))
    }
  }

  describe("#map2"){
    it("combines two Option values using a binary function"){
      assert(Option.map2(Some(1), Some(2))((a,b) => a + b) == Some(3))
    }
    it("returns none if passed a none"){
      assert(Option.map2(None, Some(2))((a,b) => a.toString + b.toString) == None)
    }
  }

  describe("#sequence"){
    it("turns a list of options into an option of a list if all values are present"){
      assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3)))
    }
    it("returns None if any of the values are None"){
      assert(Option.sequence(List(Some(1), None, Some(4))) == None)
    }
  }

  describe("#traverse"){
    it("traverses a list applying a function and returning None if any of the operations fails"){
      assert(Option.traverse(List(1, 2, 3, "a")) {
        case i: Int => Some(i)
        case _ => None
      } == None)
    }
    it("returns an option of a list if the function applies to all members"){
      assert(Option.traverse(List(1, 2, 3, 4)) {
        case i: Int => Some(i)
        case _ => None
      } == Some(List(1,2,3,4)))
    }
  }
}
