/**
  * Created by lilisun on 8/30/19.
  */
object No516 {
  object lsseq{
    def longestPalindromeSubseq(s: String): Int = {
      val n = s.length
      val dp = Array.fill(n,n)(0)
      0 until n foreach {i => dp(i)(i) = 1}
      for{
        i <- n-1 to 0 by -1
        j <- i + 1 until n
      }{
        if(s(i) == s(j)) dp(i)(j) = dp(i+1)(j-1) + 2
        else dp(i)(j) = dp(i+1)(j) max dp(i)(j-1)
      }
      dp(0)(n-1)
    }
  }

  object lsstr {
    // longest substring
    def longestPalindromeSubsStr(s: String): Int = {
      val n = s.length
      val A = Array.fill(2*n-1)('#')
      s.indices foreach {i => A(i *2) = s(i)}

      var ans = 0
      def f(i:Int):Unit = {
        (1 until n).toList.takeWhile(x => i - x >= 0 && i + x < 2* n - 2 && A(i-x) == A(i+x)) match {
          case Nil => ans = ans max 1
          case h::t => ans = ans max ((h::t).last  + 1)
        }
      }
      A.indices foreach f
      ans
    }
  }
}
