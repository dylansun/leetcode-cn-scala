/**
  * Created by lilisun on 3/30/19.
  */
object No887 {

  def superEggDrop(K: Int, N: Int): Int = {
      val mem = Array.ofDim[Int](K+1, N+1)
      //stack overflow
      def solver(k:Int, n:Int):Int = {
        if(n == 0 || k == 1 || n == 1) n
        else if(mem(k)(n) != 0) mem(k)(n)
        else {
          mem(k)(n) = (1 to n).map(x => (solver(k-1, x-1) max solver(k, n-x)) + 1).min
          mem(k)(n)
        }
      }

    val dp = Array.fill(K+1, N+1)(0)
    for ( m <-1 to N) {
      dp(0)(m) = 0
      for (k <- 1 to K) {
        dp(k)(m) = dp(k)(m-1) + dp(k-1)(m-1) + 1
        if (dp(k)(m) >= N) return m
      }
    }
    N
  }

  def main(args: Array[String]): Unit = {

    println(superEggDrop(1, 3))
  }


}
