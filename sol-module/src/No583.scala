object No583 {
  object Solution {
    def minDistance(word1: String, word2: String): Int = {
      val m = word1.length
      val n = word2.length
      val dp = Array.fill(m+1, n+1)(0)
      for{
        i <- 1 to m
        j <- 1 to n
      } if(word1(i-1) == word2(j-1)) dp(i)(j) = dp(i-1)(j-1) + 1
         else dp(i)(j) = dp(i-1)(j) max dp(i)(j-1)
      m + n - 2 * dp(m)(n)
    }
  }
}
