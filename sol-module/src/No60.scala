object No60 {
  def getPermutation(n: Int, k: Int): String = {
    val iter = (1 to n).permutations
    for( i <- 1 to k-1){
      if(iter.hasNext) iter.next()
    }
    if(iter.nonEmpty){
      return iter.next().toList.mkString
    }
    ""
  }

  def main(args: Array[String]): Unit = {
    val a1 = (1 to 4).toArray
    val itr =  a1.permutations
    println(itr.next().toList.mkString)
    println(itr.next().toList.mkString)
    println(getPermutation(4, 9))
    val a2 = (1 to 4).toArray.permutations.take(9)
    println(a2.next().mkString)
  }
}
