/**
  * Created by lilisun on 2/23/19.
  */
object No64 {
  def minPathSum(grid: Array[Array[Int]]): Int =
    if (grid.isEmpty) 0
    else {
      val m = grid.length
      val n = grid.head.length
      val pathSum = Array.fill(m, n)(0)
      for {
        i <- 0 until m
        j <- 0 until n
      } {
        if (i - 1 >= 0 && j - 1 >= 0) pathSum(i)(j) = (pathSum(i - 1)(j) min pathSum(i)(j - 1)) + grid(i)(j)
        else if (i - 1 >= 0) pathSum(i)(j) = pathSum(i - 1)(j) + grid(i)(j)
        else if (j - 1 >= 0) pathSum(i)(j) = pathSum(i)(j - 1) + grid(i)(j)
        else pathSum(i)(j) = grid(i)(j)
      }
      pathSum(m - 1)(n - 1)
    }
}
