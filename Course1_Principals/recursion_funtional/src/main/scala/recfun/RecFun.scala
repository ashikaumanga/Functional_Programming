package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c<0 || r<0) 0 else if (c == r) 1 else (pascal(c-1,r-1) + pascal(c,r-1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iter(stack: List[Char], n : Int) : Boolean = {
      if (n == chars.length) stack.isEmpty
      else if (chars(n) == '(') {
        iter(stack.prepended('('), n +1)
      } else if (chars(n) == ')') {
        if (!stack.isEmpty && stack.head == '(') {
          iter(stack.tail, n +1)
        } else {
          false
        }
      } else {
        iter(stack, n +1)
      }
    }
    iter(List(), 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iter(inHand: Int, leftCoins : List[Int]) : Int  = {
       // println(inHand)
       if (inHand==0) 1
       else if (inHand<0 || leftCoins.isEmpty) 0
       else {

         val t1 = iter(inHand - leftCoins.head, leftCoins)
         val t2 = iter(inHand, leftCoins.tail)

         t1 + t2
       }
    }
    iter(money, coins)
  }
}
