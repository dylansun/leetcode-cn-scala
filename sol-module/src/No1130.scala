/**
  * Created by lilisun on 8/21/19.
  */
object No1130 {
  object Solution {
    def mctFromLeafValues(A: Array[Int]): Int = {

      val mem_max = scala.collection.mutable.HashMap[(Int, Int), Int]()
      val mem_pro = scala.collection.mutable.HashMap[(Int, Int), Int]()
      for{
        i <- A.indices
        j <- i+1 to A.length
      }{
        val s = A.slice(i,j)
        mem_max.put((i,j-1), s.max)
      }
      val n = A.length
      val dp = Array.fill(n, n)(1000000000)
      for{
        step <- 0 until n
        i <- 0 until n
        if i + step < n
      }{
        println(i, i+step)
        if(step == 0) dp(i)(i+step) = 0
        else if(step == 1) dp(i)(i+1) = A(i)*A(i+1)
        else{
          // i, i+step
          for{
            k <- i to i+step
            if k+1 <= i + step
          }{
            dp(i)(i+step) = dp(i)(i+step) min (dp(i)(k) + dp(k+1)(i+step) + mem_max((i,k)) * mem_max((k+1, i+step)))
          }
        }
      }
      dp(0)(n-1)
    }
  }
}
