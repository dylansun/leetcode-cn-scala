/**
  * Created by lilisun on 3/23/19.
  */
object No940 {

  def distinctSubseqII(S: String): Int = {
    val pos = Array.fill(26)(0)
    val mod = 1000000007
    var cursum = 1
    S.foreach( ch => {
      val oldsum = cursum
      cursum = (cursum*2 - pos(ch - 'a') )% mod
      cursum = if(cursum < 0) cursum + mod else cursum
      println(cursum)
      pos(ch - 'a') = oldsum
    })
    cursum - 1
  }

  def main(args: Array[String]): Unit = {
   val s = "zchmliaqdgvwncfatcfivphddpzjkgyygueikthqzyeeiebczqbqhdytkoawkehkbizdmcnilcjjlpoeoqqoqpswtqdpvszfaksn"
    println(distinctSubseqII(s))
  }
}
