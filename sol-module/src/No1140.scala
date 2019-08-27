/**
  * Stone Game II
  */
object No1140 {
  object Solution {
    def stoneGameII(A: Array[Int]): Int = {
      val n = A.length
      val dp = Array.fill(51, n+1)(0)
      def gain(i:Int):Int = A.slice(i-1,A.length).sum
      for{
        i <- n to 1 by -1
        m <- 1 to 50
      }{
        if(2*m >= (n-i+1))
          dp(m)(i) = A.slice(i-1,n).sum
        else for{x <- 1 to 2*m}
          dp(m)(i) = dp(m)(i) max (gain(i) - dp((x max m) min 50)(i+x))
      }
      dp(1)(1)
    }
  }
}
