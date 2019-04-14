/**
  * Created by lilisun on 4/12/19.
  * 899. Orderly Queue
  */
object No899 {
  //  k == 1, loop
  //  k > 1, sort
  def orderlyQueue(S: String, K: Int): String = K match {
    case 1 => fmin(S)
    case _ => S.sorted
  }
  def fmin(S:String):String = {
    var res = S
    for(i <- 1 until S.length){
      val si  = (S substring (i)) + (S substring(0,i))
      if(si < res) res = si
    }
    res
  }

  def main(args: Array[String]): Unit = {
    println("abc" > "bc")
  }
}
