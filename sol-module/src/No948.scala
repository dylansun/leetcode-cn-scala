/**
  * Created by lilisun on 3/19/19.
  */
object No948 {
  def bagOfTokensScore(tokens: Array[Int], P: Int): Int = {
    var tks = tokens.sorted
    var power = P
    var score = 0
    var maxScore = 0
    while(tks.nonEmpty){
      if(power >= tks.head){
        power = power - tks.head
        score += 1
        maxScore = maxScore max score
        tks = tks.tail
      }else{
        if(score <= 0 ) return maxScore
        power += tks.last
        score -= 1
        tks = tks.dropRight(1)
      }

    }
    maxScore
  }

  def main(args: Array[String]): Unit = {
    val tokens = Array(71,55,82)
    val P = 54
    bagOfTokensScore(tokens, P)
  }
}
