package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    m <- oneOf(const[H](empty), genHeap)
  } yield insert(a, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of 1") = forAll { a: A =>
    val m = insert(a, empty)
    findMin(m) == a
  }

  property("min of 2") = forAll { (a: A, b: A) =>
    val lower = if (a < b) a else b
    val m = insert(a, insert(b, empty))
    findMin(m) == lower
  }

  property("delete min to leave empty") = forAll { a: A =>
    val m = deleteMin(insert(a,empty))
    isEmpty(m)
  }

  property("min of two melded heaps") = forAll { (h1: H, h2: H) =>
    val x = findMin(h1)
    val y = findMin(h2)
    val z = meld(h1, h2)
    findMin(z) == x || findMin(z) == y
  }

  property("always sorted queue") = forAll { heap: H =>
    def runThrough(item: A, heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val min = findMin(heap)
        val remains = deleteMin(heap)
        if (item <= min) runThrough(min, remains) else false
      }
    }
    val initMin = findMin(heap)
    val initRemain = deleteMin(heap)
    runThrough(initMin, initRemain)
  }

  property("melding keeps all elements") = forAll { (firstHeap: H, secondHeap: H) =>
    def eq(h1: H, h2: H): Boolean = if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val firstMin = findMin(h1)
      val firstRemains = deleteMin(h1)
      val secondMin = findMin(h2)
      val secondRemains = deleteMin(h2)
      if (firstMin == secondMin) eq(firstRemains, secondRemains)
      else false
    }
    val firstRemains = deleteMin(meld(firstHeap, secondHeap))
    val firstMin = findMin(firstHeap)
    val secondMin = findMin(secondHeap)
    val secondRemains = if (firstMin <= secondMin) meld(deleteMin(firstHeap), secondHeap)
    else meld(firstHeap, deleteMin(secondHeap))
    eq(firstRemains, secondRemains)
  }
}
