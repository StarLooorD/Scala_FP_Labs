package lab1

object Lab1 {
  /**
   * Трикутник Паскаля
   */
  def pascal(col: Int, row: Int): Int = {
    if (col == 0 || col == row) 1
    else pascal(col-1, row-1) + pascal(col, row-1)
  }

  /**
   * Балансування дужок
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) {
        numOpens == 0
      } else {
        val h = chars.head
        val n =
          if (h == '(') numOpens + 1
          else if (h == ')') numOpens - 1
          else numOpens
        if (n >= 0) f(chars.tail, n)
        else false
      }
    }
    f(chars, 0)
  }

  /**
   * Підрахунок решти
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
