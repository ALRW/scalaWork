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
}
