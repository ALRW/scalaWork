import org.scalatest.{FunSpec, Matchers}

class Ch2Spec extends FunSpec with Matchers {

  describe("#fibonacci") {
    it("should return the nth fibonacci number") {
      assert(Ch2.fibonacci(4) == 3)
      assert(Ch2.fibonacci(14) == 377)
    }
  }


}
