import scala.collection.mutable
object No1001 {
  def gridIllumination(N: Int, lamps: Array[Array[Int]], queries: Array[Array[Int]]): Array[Int] = {
    // check light
    val dir = Array((1,0),(1,-1),(1,1),(-1,0),(-1,1),(-1,-1),(0, -1),(0, 0),(0,1))
    (for( q <- queries) yield {
      val has_lamp = lamps.exists( x => x(0)>=0 &&
        (x(0) == q(0) || x(1) == q(1) ||
        Math.abs((x(0) - q(0)).toDouble / (x(1) - q(1)).toDouble) == 1.0))
      if(has_lamp){
        //close lamp in neighbor
        dir.foreach(x => for(idx <- lamps.indices if lamps(idx)(0) == x._1 + q(0) && lamps(idx)(1) == x._2 + q(1)) lamps(idx) = Array(-1, -1))

        1}else 0
    }).array
  }

  def main(args: Array[String]): Unit = {
    val N = 10
    val lamps = Array(Array(3,4),Array(6,6),Array(1,8), Array(4,5), Array(8,7),Array(0,6), Array(5,2), Array(1,9))
    val queries = Array(Array(7,9),Array(2,8), Array(8,6), Array(6,8), Array(2,8))
    println(gridIllumination(N,lamps,queries).mkString)

  }
}
