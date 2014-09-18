package recfun
import common._

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
    if ( c <= 0 || c > r-1) {
      1
    } else {
      pascal(c-1,r-1) + pascal(c,r-1)
    }
  }

  /**
   * Exercise 2
   */
  var balanceValue = 0

  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty && balanceValue == 0) {
      true
    } else if ((chars.isEmpty && balanceValue != 0) || balanceValue < 0) {
      balanceValue = 0
      false
    } else {
      if (chars.head == '(') balanceValue = balanceValue + 1
      if (chars.head == ')') balanceValue = balanceValue - 1
      balance(chars.tail)  
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else if (coins.isEmpty && money >0) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
