/**
  * Created by lilisun on 3/5/19.
  */
object No935 {
  def knightDialer(N: Int): Int = {
    val mod = 1e9.toInt + 7
    val dp = Array.ofDim[Int](10, N) // dp(x)(y): x: start num, y: steps
    for(i <- 0 to 9) dp(i)(0) = 1
    for(step <- 1 until N){
      dp(0)(step) = dp(4)(step -1) + dp(6)(step -1)
      dp(1)(step) = dp(6)(step -1) + dp(8)(step -1)
      dp(2)(step) = dp(7)(step -1) + dp(9)(step -1)
      dp(3)(step) = dp(8)(step -1) + dp(4)(step -1)

      dp(4)(step) = (dp(3)(step -1) + dp(9)(step -1)) % mod + dp(0)(step -1)
      dp(5)(step) = 0
      dp(6)(step) = (dp(7)(step -1) + dp(1)(step -1)) % mod + dp(0)(step -1)
      dp(7)(step) = dp(2)(step -1) + dp(6)(step -1)
      dp(8)(step) = dp(1)(step -1) + dp(3)(step -1)
      dp(9)(step) = dp(2)(step -1) + dp(4)(step -1)

      for(i <- 0 to 9) dp(i)(step) = dp(i)(step) % mod

    }
    var ans = 0
    for(i <- 0 to 9) ans = (ans + dp(i)(N-1)) % mod
    ans
  }

  def main(args: Array[String]): Unit = {
    println(1e9.toInt)
    println(knightDialer(161))
    println(Int.MaxValue)

  }
}
