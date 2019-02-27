/**
  * Created by lilisun on 2/27/19.
  */
object No463 {
  def islandPerimeter(grid: Array[Array[Int]]): Int = {
    def f(z:Int, y:Int):Int = {
      val n = List((1,0), (0,1),(-1,0), (0,-1))
      var ans = 4
      n.foreach(dx => {
        val x = (z + dx._1, y + dx._2)
        if(inBound(x._1,x._2)){
          if(grid(x._1)(x._2) == 1) ans -= 1
        }
      })
      ans
    }
    def inBound(x:Int,y:Int):Boolean = {
      x >= 0 && y >= 0 && x  < grid.length && y < grid(0).length
    }

    var ans = 0
    for(x <- grid.indices) for(y <- grid(0).indices){
      if(grid(x)(y) == 1) ans += f(x,y)
    }
    ans
  }
}
