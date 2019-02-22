
object No221 {
  def maximalSquare(matrix: Array[Array[Char]]): Int =
    if (matrix.isEmpty || matrix.head.isEmpty) 0
    else {
      val height = matrix.length
      val width = matrix.head.length
      val dp = Array.fill(height + 1, width + 1)(0)
      var res = 0
      for {
        i <- 1 to height
        j <- 1 to width
      } {
        if (matrix(i - 1)(j - 1) == '0') dp(i)(j) = 0
        else {
          dp(i)(j) = Math.min(Math.min(dp(i - 1)(j), dp(i)(j - 1)), dp(i - 1)(j - 1)) + 1
          res = Math.max(res, dp(i)(j))
        }
      }
      res*res
    }
}
