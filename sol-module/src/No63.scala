/**
  * Created by lilisun on 2/23/19.
  */
object No63 {
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int =
    if (obstacleGrid.isEmpty) 0
    else {
      val m = obstacleGrid.length
      val n = obstacleGrid.head.length
      val grid = Array.fill(m, n)(0)
      for {
        i <- 0 until m
        j <- 0 until n
        if obstacleGrid(i)(j) == 0
      } {
        if (i - 1 >= 0 && j - 1 >= 0) grid(i)(j) += grid(i - 1)(j) + grid(i)(j - 1)
        else if (i - 1 >= 0) grid(i)(j) += grid(i - 1)(j)
        else if (j - 1 >= 0) grid(i)(j) += grid(i)(j - 1)
        else grid(i)(j) = 1
      }
      grid(m - 1)(n - 1)
    }
}
