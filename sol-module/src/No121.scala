/**
  * Created by lilisun on 1/10/19.
  */
object No121 {
  def maxProfit(prices: Array[Int]): Int = {
    var maxprofit = 0
    for (i <- 0 to prices.length - 2) {
      for ( j <- i + 1 to  prices.length -1) {
        val profit = prices(j) - prices(i)
        if (profit > maxprofit)
          maxprofit = profit
      }
    }
    return maxprofit
  }
}
