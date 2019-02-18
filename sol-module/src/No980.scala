/**
  * Created by lilisun on 2/18/19.
  */
object No980 {

  def uniquePathsIII(grid: Array[Array[Int]]): Int = {
    val start = findstart(grid)
    println("------- grid ------------")
    printgrid(grid)
    val new_grid = grid
    new_grid(start._1)(start._2) = -1
    val dir = List((start._1+1, start._2),
        (start._1-1, start._2),
        (start._1, start._2+1),
        (start._1, start._2-1)
    )
    var ans = 0
    val row = grid.length - 1
    val col = grid(0).length - 1

    dir.foreach(x =>{
      println(s"x: $x")
      if(x._1 >=0 && x._1 <= row && x._2 >=0 && x._2 <= col ){
       if(new_grid(x._1)(x._2) == 0){
         new_grid(x._1)(x._2) = 1
         println("-------new grid ------------")
         printgrid(new_grid)
         ans += uniquePathsIII(new_grid)
         new_grid(x._1)(x._2) =0
       }
        if(new_grid(x._1)(x._2) == 2){
          if(isnozero(new_grid))
            ans += 1
        }
      }
    })

    ans
  }

  def printgrid(grid: Array[Array[Int]]): Unit = {
    for(x <- grid.indices) {
      for(y <- grid(x).indices) print(s"${grid(x)(y)} ")
      println()
    }
  }

  def findstart(grid: Array[Array[Int]]): (Int, Int)= {
    for( x <- grid.indices) for( y <- grid(x).indices if grid(x)(y) == 1) return (x, y)
    (-1, -1)
  }

  def isnozero(grid:Array[Array[Int]]): Boolean = {
    for(x<- grid.indices) for(y <- grid(x).indices if grid(x)(y) == 0) return false
    true
  }

  def main(args: Array[String]): Unit = {
    val t1 = Array(Array(1,0,0,0), Array(0,0,0,0), Array(0,0,2,-1))
    println(uniquePathsIII(t1))
  }
}
