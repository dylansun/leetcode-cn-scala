/**
  *
  *  1139. Largest 1-Bordered Square
  */
object No1139 {
  object Solution {
    def largest1BorderedSquare(grid: Array[Array[Int]]): Int = {
      // ___________
      // |         |
      // |         |
      // ___________

      def f(i:Int, j:Int, k:Int):Boolean = {
        for{x <- j until j + k if grid(i)(x) == 0} return false
        for{x <- j until j + k if grid(i+k-1)(x) == 0} return false
        for{y <- i until i + k if grid(y)(j) == 0} return false
        for{y <- i until i + k if grid(y)(j+k-1) == 0} return false
        true
      }
      val n = grid.length
      val m = grid(0).length

      for{
        k <- (n min m) to 1 by -1
        i <- grid.indices
        j <- grid(i).indices
        x = i+k-1
        y = j+k-1
        if x < n && y < m
      }{
        if(f(i,j,k)) return k*k
      }
      0
    }
  }
}
