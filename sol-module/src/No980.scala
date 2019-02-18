/**
  * Created by lilisun on 2/18/19.
  */
object No980 {

  def uniquePathsIII(grid: Array[Array[Int]]): Int = {
    val start = findstart(grid)
    grid(start._1)(start._2) = -1
    val dir = List((start._1+1, start._2),
        (start._1-1, start._2),
        (start._1, start._2+1),
        (start._1, start._2-1)
    )

    var ans = 0
    dir.foreach(x =>{
      if(x._1 >=0 && x._1 < grid.length && x._2 >=0 && x._2 < grid(0).length ){
       if(grid(x._1)(x._2) == 0){
         grid(x._1)(x._2) = 1
         ans += uniquePathsIII(grid)
         grid(x._1)(x._2) =0
       }
        if(grid(x._1)(x._2) == 2 && isnozero(grid)) ans += 1
      }
    })

    ans
  }
  def findstart(grid: Array[Array[Int]]): (Int, Int)= {
    for( x <- grid.indices) for( y <- grid(x).indices if grid(x)(y) == 1) return (x, y)
    (-1,-1)//remove this line, show compile error
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
