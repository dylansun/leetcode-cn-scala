import scala.collection.mutable
object No1001 {
  def gridIllumination(N: Int, lamps: Array[Array[Int]], queries: Array[Array[Int]]): Array[Int] = {
    // check light
    val dir = Array((1,0),(1,-1),(1,1),(-1,0),(-1,1),(-1,-1),(0, -1),(0, 0),(0,1))
    (for( q <- queries) yield {
      val has_lamp = lamps.exists( x => x(0)>0 &&
        (x(0) == q(0) || x(1) == q(1) ||
        Math.abs((x(0) - q(0)).toDouble / (x(1) - q(1)).toDouble) == 1.0))
      if(has_lamp){
        //close lamp in neighbor
        dir.foreach(x => for(idx <- lamps.indices if lamps(idx)(0) == x._1 + q(0) && lamps(idx)(1) == x._2 + q(1)) lamps(idx) = Array(-1, -1))
        1}else 0
    }).array
  }

  def main(args: Array[String]): Unit = {
    val N = 5
    val lamps = Array(Array(0,0),Array(4,4))
    val queries = Array(Array(1,1),Array(1,0))
    println(gridIllumination(N,lamps,queries).mkString)
  }
}
