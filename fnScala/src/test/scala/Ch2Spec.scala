import org.scalatest.{FunSpec, Matchers}

class Ch2Spec extends FunSpec with Matchers {

  describe("#fibonacci") {
    it("should return the nth fibonacci number") {
      assert(Ch2.fibonacci(4) == 3)
      assert(Ch2.fibonacci(14) == 377)
    }
  }

  describe("#isSorted") {
    it("should return false if an array is not sorted according to a given function") {
      val a = Array[Int](1,3,5,4,2,6,9,2)
      val fn = (a:Int,b:Int) => a > b
      assert(!Ch2.isSorted(a, fn))
    }
    it("should return true if an array is sorted according to a given function") {
      val a = Array[String]("a", "b", "c")
      val fn = (a: String, b: String) => a < b
      assert(Ch2.isSorted(a,fn))
    }
  }

  describe("#curry") {
    it("should take a function of two arguments and returns a partially applied function"){
      val fn1 = (a: Int, b:Float) => a.toByte + b.toByte
      val fn2 = (a: Int) => (b: Float) => a.toByte + b.toByte
      assert(Ch2.curry(fn1)(1)(2) == fn2(1)(2))
    }
  }

  describe("#uncurry"){
    it("should take a curried function and return a standard function") {
      val fn1 = (a: Int, b:Float) => a.toByte + b.toByte
      val fn2 = (a: Int) => (b: Float) => a.toByte + b.toByte
      assert(Ch2.uncurry(fn2)(1,2) == fn1(1,2))
    }
  }

  describe("#compose"){
    it("performs function composition when given two functions") {
      val fn3 = (a: Int) => a + 1
      val fn4 = (b: Int) => b + 3
      val fn5 = (c: Int) => c + 1 + 3
      assert(Ch2.compose(fn3, fn4)(1) == fn5(1))
    }
  }

}
