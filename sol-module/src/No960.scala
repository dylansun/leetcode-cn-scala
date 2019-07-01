/**
  * Created by lilisun on 3/5/19.
  */
object No960 {
  def minDeletionSize(A: Array[String]): Int = {
    val num = A.length
    val dp = Array.fill(A(0).length)(1)
    for(i <- A(0).indices)
      for(j <- 0 until i)
        if(A.forall( x=> x(j) <= x(i)))
          dp(i) = dp(i) max  (dp(j) + 1)
    return A(0).length - dp.max
  }


  object Solution {
    def maxIncreaseKeepingSkyline(grid: Array[Array[Int]]): Int = {
      val A = grid map (_.max) // row
      val B = grid(0).indices.toArray.map(j => grid.indices.map(i => grid(i)(j)).max)
      val d = {
        for{
          i <- grid.indices
          j <- grid(i).indices
        } yield (A(i) min B(j)) - grid(i)(j)
      }
      d.sum
    }
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(Array(1,1))
    println(Solution.maxIncreaseKeepingSkyline(grid))
  }
}
