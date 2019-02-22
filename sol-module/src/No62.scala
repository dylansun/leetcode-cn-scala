/**
  * Created by lilisun on 2/23/19.
  */
object No62 {
  def uniquePaths(m: Int, n: Int): Int = {
    val grid = Array.fill(m, n)(0)
    grid(0)(0) = 1
    for {
      i <- 0 until m
      j <- 0 until n
    } {
      if (i - 1 >= 0) grid(i)(j) += grid(i - 1)(j)
      if (j - 1 >= 0) grid(i)(j) += grid(i)(j - 1)
    }
    grid(m - 1)(n - 1)
  }
}
