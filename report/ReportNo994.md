#### No. 994 Rotting Oranges

1. 寻找要变化的橘子的坐标
2. 如果存在 修改橘子
3. 如果不存在, 检查是否全部为rotten

```Scala
object Solution {
  def orangesRotting(grid: Array[Array[Int]]): Int = solver(grid, 0)

  def solver(grid: Array[Array[Int]], acc: Int): Int = {
    //Step 1
    val tbr = (for(x <- grid.indices) yield {
      (for(y <- grid(0).indices if grid(x)(y) == 1 && isAdjRot( (x, y), grid)) yield (x, y)).toList
    }).toList.reduce(_:::_)

    //Step 2 and 3
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
}
```

