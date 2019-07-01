/**
  * Created by lilisun on 5/5/19.
  */
object contest135_3 {
  def minScoreTriangulation(A: Array[Int]): Int = {
    val n = A.length
    if(A.length < 3) 0
    else if(A.length == 3) A.product
    else{
      val n = A.length
      val B = A++A
      val dp = Array.fill(2*n, 2*n)(Int.MaxValue)
      for{
        i <- 0 until 2*n
        j <- i+1 until 2*n
      }{
        if (j - i == 1) dp(i)(j) = 0
        if (j - i == 2) dp(i)(j) = f(B)(i,i+1,i+2)
      }
      for{
        len <- 2 to n
        i <- 0 until 2*n
        j = i + len
        if j < 2*n
      }{
        for{p <- i+1 until j}
        dp(i)(j) = dp(i)(j) min (dp(i)(p) + dp(p)(j) + f(B)(i,p,j))
      }
      (0 until n).toList.map( i => dp(i)(i+n-1)).min
    }
  }
  def f(B:Array[Int])(i:Int, j:Int, k:Int):Int = B(i) * B(j) * B(k)

  def main(args: Array[String]): Unit = {
    val A = Array(1,3,1,4,1,5)
    println(minScoreTriangulation(A))
  }
}
