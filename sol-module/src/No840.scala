/**
  * Created by lilisun on 2/27/19.
  */
object No840 {
  def numMagicSquaresInside(grid: Array[Array[Int]]): Int = {
    def f(acc:Int, pos: (Int, Int)): Int ={
      val p = (for(x <- 0 to 2)yield {for( y <- 0 to 2) yield grid(pos._1 +x)(pos._2 +y)}).flatten.toList
      val ps = p.distinct.sorted
      if(ps ==  (1 to 9).toList){
        val d =   p(0) + p(4) + p(8)
        val sig = p(2) + p(4) + p(6) == d &&
                  p(0) + p(1) + p(2) == d &&
                  p(3) + p(4) + p(5) == d &&
                  p(6) + p(7) + p(8) == d &&
                  p(0) + p(3) + p(6) == d &&
                  p(1) + p(4) + p(7) == d &&
                  p(2) + p(5) + p(8) == d
        if(sig)  acc + 1 else  acc
      }else acc

    }

    val n = grid.length
    val m = grid(0).length
    if(n < 3 || m < 3) 0
    else{
      val idx = (for( x<- 0 until n-2)yield{List(x).zipAll(0 until m-2,x,x)}).flatten.toList
      idx.foldLeft(0)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    val t = Array(Array(4,3,8,4), Array(9,5,1,9), Array(2,7,6,2))
    println(numMagicSquaresInside(t))
    println((1 to 9).toList)
  }
}
