/**
  * Created by lilisun on 3/30/19.
  */
object No903 {

  // f(n)(0) =
  def numPermsDISequence(S: String): Int = {
    val mod = Math.pow(10, 9).toInt + 7
    val dp = Array.fill(S.length + 1, S.length + 1)(0)
    dp(0)(0) = 1
    for{ i <- S.indices} S(i) match {
        case 'I' => for(j <- 1 to i + 1) dp(i+1)(j) = (dp(i+1)(j-1) + dp(i)(j-1)) % mod
        case 'D' => for(j <- i to 0 by -1) dp(i+1)(j) = (dp(i+1)(j+1) + dp(i)(j)) % mod
    }
    (0 to S.length)
      .map(dp(S.length))
      .foldLeft(0){(sum, x) => (sum + x) % mod}
  }

  def main(args: Array[String]): Unit = {

    val s = "DID"
    println(numPermsDISequence(s))
  }

}
