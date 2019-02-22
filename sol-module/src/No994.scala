object No994 {
  /**
    1 <= grid.length <= 10
    1 <= grid[0].length <= 10
    grid[i][j] is only 0, 1, or 2.
   */
  def orangesRotting(grid: Array[Array[Int]]): Int = solver(grid, 0)

  def solver(grid: Array[Array[Int]], acc: Int): Int = {
    val tbr = (for(x <- grid.indices) yield {
      (for(y <- grid(0).indices if grid(x)(y) == 1 && isAdjRot( (x, y), grid)) yield (x, y)).toList
    }).toList.reduce(_:::_)

    if(tbr.isEmpty){
      if(grid.map(x => x.contains(1)).reduce(_||_)) -1 else acc
    }else{
      tbr.foreach( x => grid(x._1)(x._2) = 2)
      solver(grid, acc + 1)
    }
  }

  def isAdjRot(pos: (Int, Int), grid: Array[Array[Int]]): Boolean = {
    val offset = Array((1,0), (0,1), (-1, 0), (0, -1))
    (for(x <- offset ) yield{
      val np = (pos._1 + x._1, pos._2 + x._2)
      if(isInbound(np, grid)) grid(np._1)(np._2) == 2 else false
    }).reduce(_||_)
  }

  def isInbound(np: (Int, Int), grid: Array[Array[Int]]): Boolean = {
    np._1 >= 0 && np._2 >= 0 && np._1 < grid.length && np._2 < grid(0).length
  }

  def main(args: Array[String]): Unit = {
    //val x = (for(i <- 0 to 6) yield {(for(j <- 0 to 7) yield (i, j)).toList}).toList.reduce(_:::_)
    val g1 = Array( Array(2,1,1), Array(1,1,0), Array(0,1,1))
    val g2 = Array( Array(2,1,1), Array(0,1,1), Array(1,0,1))
    println(orangesRotting(g1))
    println(orangesRotting(g2))

  }
}
