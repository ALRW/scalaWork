package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      def factorial(x: Int): Int =  if(x <= 1) 1 else x * factorial(x -1)

      factorial(r) / (factorial(c) * factorial(r - c))

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def balanced(chars: List[Char], open: Int): Boolean = {
        if(chars.isEmpty) open == 0
        else if(chars.head == '(') balanced(chars.tail, open + 1)
        else if(chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
        else balanced(chars.tail, open)
      }

    balanced(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

    def recurs(x: Int, cs: List[Int], count: Int): Int = {
      if(x < 0) count
      else if(cs.isEmpty){
        if(x == 0) count + 1 else count
      }
      else recurs(x, cs.tail, count) + recurs(x - cs.head, cs, count)
    }
    recurs(money, coins, 0)
  }
  }
