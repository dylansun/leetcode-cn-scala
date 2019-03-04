/**
  * Created by lilisun on 3/5/19.
  */
object No931 {
  def minFallingPathSum(A: Array[Array[Int]]): Int = {
    val n = A.length
    if(n == 0) return 0
    if(n == 1) return A(0)(0)

    var dp = Array.ofDim[Int](n)

    for(i <- dp.indices) dp(i) = A(0)(i)
    for(level <- 1 until n){
      val tmp = Array.ofDim[Int](n)
      for(x <- dp.indices) {
        x match {
          case 0 => tmp(x) = (dp(0) min dp(1)) + A(level)(x)
          case s:Int if s == n-1 => tmp(x)=(dp(x-1) min dp(x)) + A(level)(x)
          case _ => tmp(x) = (dp(x-1) min dp(x) min dp(x+1)) + A(level)(x)
        }
      }
      dp = tmp
    }
    dp.min
  }
}
