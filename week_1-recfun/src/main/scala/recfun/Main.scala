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
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0) throw new IllegalArgumentException("Cannot have a negative row or column")
    else {
      if (c > r) throw new IllegalArgumentException("The column cannot be greater than the row, otherwise it would be " +
        "Pascal's Polygon, not Pascal's Triangle")
      else {
        if (c == 0 || c == r) 1
        else pascal(c, r - 1) + pascal(c - 1, r - 1)
      }
    }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def matched(l_parens: Int, chars: List[Char]): Boolean =
      // A string is only balanced if there are no unmatched left parenthesis
      if (chars.isEmpty) {
        if (l_parens == 0) true
        else false
      }

      else {
        if (chars.head == '(') matched(l_parens + 1, chars.tail)

        else {
          if (chars.head == ')') {
            if (l_parens > 0) matched(l_parens - 1, chars.tail)
            else false
          }

          // leading character is a non-paren character
          else matched(l_parens, chars.tail)
        }
      }

    matched(0, chars)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    // You can always change no money, regardless of how many coins you have
    if (money == 0) 1
    else {
      if (coins.isEmpty) 0
      else {
        // Success! Money changed, now add all the remaining combinations
        if (money == coins.head) 1 + countChange(money, coins.tail)
        else {
          if (money < coins.head) countChange(money, coins.tail)
          else {
            countChange(money - coins.head, coins) + countChange(money, coins.tail)
          }
        }
      }
    }



}
