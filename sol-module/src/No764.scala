/**
  * Created by lilisun on 3/5/19.
  */
object No764 {
  def minCostClimbingStairs(cost: Array[Int]): Int = {
    val dp = Array.ofDim[Int](cost.length+1)
    for(x <- dp.indices) {
      x match {
        case 0 => dp(0) = 0
        case 1 => dp(1) = 0 //你可以选择从索引为 0 或 1 的元素作为初始阶梯。
        case _ =>  dp(x) = (dp(x-2)+cost(x-2)) min (dp(x-1) + cost(x-1))
      }
    }
    dp.last
  }
}
