import org.scalatest.{FunSpec, Matchers}

class TreeSpec extends FunSpec with Matchers {

  describe("#size"){
    it("should return the number of nodes (branches and leaves on a tree"){
      assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5)
    }
    it("should work for larger Trees"){
      assert(Tree.size(Branch(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4)), Leaf(5))) == 9)
    }
  }

}
