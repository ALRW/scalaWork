package recfun

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

      factorial(r) / (factorial(c) * (factorial(r - c)))

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], open: Int): Boolean = {
        if(chars.isEmpty) open == 0
        else
          if(chars.head == '(') balanced(chars.tail, open + 1)
          else
            if(chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
            else balanced(chars.tail, open)
      }
    balanced(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    def recurs(money: Int, coins: List[Int], count: Int): Int = {
      if(money < 0) count
      else if(coins.isEmpty){
        if(money == 0) count + 1 else count
      }
      else recurs(money, coins.tail, count) + recurs(money - coins.head, coins, count)
    }
    recurs(money, coins, 0)
  }
  }
