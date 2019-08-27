/**
  * Created by lilisun on 8/22/19.
  * 1043. 分隔数组以得到最大和
  */
object No1043 {
  object Solution {
    def maxSumAfterPartitioning(A: Array[Int], K: Int): Int = {
      val dp = Array.fill(A.length+1)(0)
      dp(0) = 0
      for{i <- 1 to A.length}{
        for{j <- 1 to K if i - j >= 0}
          dp(i) = dp(i) max (dp(i-j) + A.slice(i-j, i).max * j)
      }
      dp(A.length)
    }
  }
}
