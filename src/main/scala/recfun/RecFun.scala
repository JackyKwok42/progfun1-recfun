package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 | c==r)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balance_count(chars: List[Char], count: Int): Int = {
      if(chars.isEmpty | count < 0)
        count
      else{
        if( chars.head == '(')
          balance_count(chars.tail, count + 1)
        else if (chars.head == ')')
          balance_count(chars.tail, count - 1)
        else
          balance_count(chars.tail, count)
      }
    }

    balance_count(chars,0) == 0
//    count = 0
//    if chars.isEmpty then false
//    else{
//      val word_list = chars.toList
//      word_list.foreach(_.)
//    }
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 & coins.isEmpty)
      0
    else if (money == 0)
      1
    else
      countChange(money- coins.head,coins) + countChange(money, coins.tail)
  }
