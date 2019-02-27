/**
  * Created by lilisun on 2/28/19.
  */
object No329 {
  def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
    if(matrix.length == 0 || matrix(0).length == 0) return 0
    val dp = Array.ofDim[Int](matrix.length, matrix(0).length)
    for(x <- matrix.indices) for(y <- matrix(0).indices) dp(x)(y) = 0

    val tb = scala.collection.mutable.HashMap[Int, List[(Int, Int)]]()
    val l = matrix.flatten.sorted.distinct.reverse
    for(x <- matrix.indices) for(y <- matrix(0).indices) {
      tb.put(matrix(x)(y), (x,y)::tb.getOrElse(matrix(x)(y), List[(Int, Int)]()))
    }

    def doDP(l: Array[Int]): Unit = {
      if(l.length > 0){
        for(x <- tb.get(l.head).get) update(x)
        doDP(l.tail)

      }
    }

    def update(x:(Int, Int)):Unit = {
      val step = List((-1, 0),(1,0),(0,1),(0,-1))
      var delta = 0
      for(dx <- step){
        val nx = (x._1 + dx._1, x._2 + dx._2)
        if(inBound(nx, matrix) && matrix(nx._1)(nx._2) > matrix(x._1)(x._2)){
          delta = delta max dp(nx._1)(nx._2)
        }
      }
      dp(x._1)(x._2) = 1 + delta
    }

    doDP(l)
    dp.flatten.max
  }

  def inBound(x: (Int, Int), matrix: Array[Array[Int]]):Boolean = {
    x._1 >=0 && x._2 >=0 && x._1 < matrix.length && x._2 < matrix(0).length
  }

}
