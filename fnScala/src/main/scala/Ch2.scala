import scala.annotation.tailrec

object Ch2 {

  def fibonacci(n: Int): Int = {
    @tailrec
    def increment(inc: Int, previous: Int, current: Int): Int ={
      if(inc >= n) current
      else increment(inc + 1, current, previous + current)
    }
    increment(1,0,1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as sameElements as.sortWith(ordered)
  }

  def curry[A,B,C](fn: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => fn(a,b)
  }

  def uncurry[A,B,C](fn: A => B => C): (A, B) => C = {
    (a: A, b: B) => fn(a)(b)
  }

  def compose[A,B,C](fn: B => C, fn2: A => B): A => C = {
    (a: A) => fn(fn2(a))
  }
}
