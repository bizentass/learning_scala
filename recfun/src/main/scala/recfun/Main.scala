package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
//
//    println(balance("(just an example))".toList))

    print(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if( c == 0) 1 else if(r == 0) 0 else pascal(c,r-1) + pascal(c-1,r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], level: Int): Boolean=
      {
        if(level >= 0) {
          if (chars.isEmpty)
            level == 0
          else if (chars.head == '(')
            balanced(chars.tail, level + 1)
          else if (chars.head == ')')
            balanced(chars.tail, level - 1)
          else
            balanced(chars.tail, level)
        }
        else
          false
      }

      balanced(chars, 0)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        return 1 // only one way of making money 0 i.e all denominations should be zero */
      if (money <= 0)
        return 0
      if(!coins.isEmpty) {
        if (coins.head <= 0 && money >= 1)
          return 0
        else
          return countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
      else
        return 0
    }
  }
