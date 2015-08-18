package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c <= 0 || c >= r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceRec(currentChars: List[Char], count: Int): Boolean = {
      if (count < 0)
        false
      else if (currentChars.isEmpty)
        count == 0
      else if (currentChars.head == '(')
        balanceRec(currentChars.tail, count + 1)
      else if (currentChars.head == ')')
        balanceRec(currentChars.tail, count - 1)
      else
        balanceRec(currentChars.tail, count)
    }

    balanceRec(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRec(currentMoney: Int, currentCoins: List[Int]): Int = {
      if (currentMoney == money)
        1
      else if (currentMoney > money || currentCoins.isEmpty)
        0
      else
        countChangeRec(currentMoney + currentCoins.head, currentCoins) +
          countChangeRec(currentMoney, currentCoins.tail)
    }

    countChangeRec(0, coins)
  }
}
