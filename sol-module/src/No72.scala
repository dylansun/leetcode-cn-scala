/**
  * Created by lilisun on 3/23/19.
  */
object No72 {
  def minDistance(word1: String, word2: String): Int = {
    val n = word1.length; val m = word2.length
    val dp = Array.fill(n+1, m+1)(0)
    for(i <- 0 to n){
      for(j <- 0 to m){
        if(i == 0 || j == 0){
          dp(i)(j) = i max j
        }
        else{
          word1(n-i) == word2(m-j) match {
            case true => dp(i)(j) = dp(i-1)(j-1)
            case false => dp(i)(j) = 1+ (dp(i-1)(j) min dp(i)(j-1) min dp(i-1)(j-1))
          }
        }
      }
    }
    dp(n)(m)
  }

  def maxCommonSubsequece(word1: String, word2: String): Int = {
    val n = word1.length; val m = word2.length
    val dp = Array.fill(n+1, m+1)(0)
    for(i <- 1 to n){
      for(j <- 1 to m){
          word1(n-i) == word2(m-j) match {
            case true => dp(i)(j) = 1 + dp(i-1)(j-1)
            case false => dp(i)(j) = dp(i-1)(j) max dp(i)(j-1)
        }
      }
    }
    dp(n)(m)
  }

  def main(args: Array[String]): Unit = {
    println(1 + 6 min 3 min 3)
  }
}
