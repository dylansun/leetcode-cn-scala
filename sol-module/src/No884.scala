object No884 {
  def uncommonFromSentences(A: String, B: String): Array[String] = {
    (A+" "+B).split(" ").map((_,1)).groupBy(_._1).filter(_._2.length == 1)
    null
  }

  def main(args: Array[String]): Unit = {
    val A = "pig too pig"
    val B = "apple too apl"
    val C =  (A+" "+B).split(" ").map((_,1)).groupBy(_._1).filter(_._2.length == 1).map(_._1).toArray
    println(C)
  }
}
