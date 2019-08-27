/**
  * 44. Wildcard Matching
  */
object No44 {
  object Solution1 {
    def isMatch(s: String, p: String): Boolean = {
      s matches p
        .replaceAll("[\\*]+", "\\*")
        .replaceAll("\\*", "[a-zA-Z]\\*")
        .replaceAll("\\?","[A-Za-z]")
    }
  }

  object Solution {
    def isMatch(s: String, p: String): Boolean = {
      val str = s.toArray
      val pattern = p.toArray

      val n = str.length
      val m = pattern.length
      val dp = Array.fill(n+1,m+1)(false)
      dp(0)(0) = true

      def f(i:Int):Unit = if(i <= m && pattern(i-1) == '*') {dp(0)(i) = true;f(i+1)}
      f(1)

      for{
        i <- 1 to n
        j <- 1 to m
      }{
        if(str(i-1)== pattern(j-1) || pattern(j-1) == '?')
          dp(i)(j) = dp(i-1)(j-1)
        if(pattern(j-1) == '*')
          dp(i)(j) = dp(i-1)(j) || dp(i)(j-1) || dp(i-1)(j-1)
      }

      dp(n)(m)
    }
  }
}
