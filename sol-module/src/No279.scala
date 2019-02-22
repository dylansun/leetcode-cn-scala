
object No279 {
  def numSquares(n: Int): Int =
    if (n <= 1) n
    else {
      val dp = Array.fill(n + 1)(Int.MaxValue)
      dp(0) = 0
      dp(1) = 1
      (2 to n).foreach { i =>

        (1 until i).takeWhile(j => j * j <= i)
          .foreach(j => dp(i) = Math.min(dp(i), dp(i - j * j) + 1))
      }
      dp(n)
    }
}

