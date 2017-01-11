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

  describe("#maximum"){
    it("Should return the highest value in a Tree[Int]"){
      assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
    }
  }

  describe("#depth"){
    it("Should return the maximum depth of a Tree[A]"){
      assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 2)
    }
  }

  describe("#map"){
    it("Should apply a given function to each element of a Tree"){
      assert(Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(a => a + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
    }
  }


  describe("#fold"){
    it("Should generalise all the above functions"){
      assert(Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(x => 1)(1 + _ + _) == 5)
    }
  }
}
