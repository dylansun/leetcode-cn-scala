/**
  * Created by lilisun on 1/12/19.
  */
object No874 {
  def robotSim(commands: Array[Int], obstacles: Array[Array[Int]]): Int = {
    val dx = Array(0, 1, 0, -1)
    val dy = Array(1, 0,-1,  0)
    var x = 0; var y =0; var di = 0; var ans = 0

    val obst = scala.collection.mutable.Set[(Int, Int)]()
    for(i <- 0 to obstacles.length -1 ) obst.add((obstacles(i)(0), obstacles(i)(1)))
    for(cmd <- commands){
      cmd match {
        case -1 => di = ((di + 1) % 4)
        case -2 => di = ((di - 1 + 4) % 4)
        case _  => {
          println(cmd)
          for(step <- 0 to cmd - 1){
            val next = ((x + dx(di), y + dy(di)))
            println(obst.contains(next))
            if( !obst.contains((x + dx(di), y + dy(di)))){
              x = x + dx(di)
              y = y + dy(di)
              ans = ans max (x*x+y*y)
            }
          }
        }
      }
    }
    return ans
  }

  def main(args: Array[String]): Unit = {
    val commands = Array(-2,8,3,7,-1)
    val obstacles = Array(
      Array(-4,-1),
      Array(1,-1),
      Array(1,4),
      Array(5,0),
      Array(4,5),
      Array(-2,-1),
      Array(2,-5),
      Array(5,1),
      Array(-3,-1),Array(5,-3))

    val dx = Array(0, 1, 0, -1)
    val dy = Array(1, 0,-1,  0)
    println(dx(1))
    var di = 0
    di = (di - 5) % 4
    println(di)
    robotSim(commands, obstacles)
  }
}
