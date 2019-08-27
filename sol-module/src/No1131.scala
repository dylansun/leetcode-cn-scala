/**
  * Maximum of Absolute Value Expression
  */
object No1131 {
  object Solution {
    def maxAbsValExpr(A: Array[Int], B: Array[Int]): Int = {
      def abs(x:Int):Int = Math.abs(x)
      val n = A.length
      var m = Int.MinValue
      for{
        i <- 0 until n
        j <- i+1 until n
      } {
        val t = j-i + abs(A(i) - A(j)) + abs(B(i) - B(j))
        m = m max t
      }
      m
    }
  }
}
