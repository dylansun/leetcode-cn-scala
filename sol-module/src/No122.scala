/**
  * Created by lilisun on 1/10/19.
  */
object No122 {
  def maxProfit(prices: Array[Int]): Int = {
    (for(i <- 0 to prices.length-2) yield prices(i+1) -prices(i) ).filter(_ > 0).sum
  }

}
