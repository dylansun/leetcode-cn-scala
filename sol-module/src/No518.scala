/**
  * 518. 零钱兑换 II
  */
object No518 {
  object Solution {
    def change(n: Int, xs: Array[Int]): Int = {
      val dp = Array.fill(n+1)(0)
      dp(0) = 1
      for{
        x <- xs
        m <- 1 to n
        if m >= x
      } dp (m) = dp(m) + dp(m - x)
      dp.last
    }
  }
}
