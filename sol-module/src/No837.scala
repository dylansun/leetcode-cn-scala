/**
  * Created by lilisun on 4/24/19.
  */
object No837 {
  def f(N:Int, K:Int, W:Int): Double ={
    val dp = Array.fill(K+W)(0.0)
    dp(0) = 1.0
    for{i <- 1 until K+W}{
      dp(i) = (i - W to i -1).toList.filter(_ >= 0).filter(_ < K).map(dp).sum / W
    }
    println(dp.toList)
    (0 until K+W).toList.filter(x => x >= K && x <= N).map(dp).sum

  }

  def main(args: Array[String]): Unit = {

    println(f(21,17,10))
  }
}
