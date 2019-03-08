/**
  * Created by lilisun on 3/8/19.
  */
import scala.collection.mutable
object No963 {


  def area(i: Int, j: Int, k: Int, points: Array[Array[Int]]):Double = {
    val  v1 = (points(i)(0) - points(j)(0), points(i)(1) - points(j)(1))
    val  v2 = (points(i)(0) - points(k)(0), points(i)(1) - points(k)(1))
    lenOfVector(v1) * lenOfVector(v2)
  }
  def minAreaFreeRect(points: Array[Array[Int]]): Double = {
    val N = points.length

    val pointSet = mutable.HashSet[(Int, Int)]()
    points.foreach(x => pointSet += ((x(0),x(1))))

    var ans = Double.MaxValue
    for ( i <- 0 until N) {

      for (j <- 0 until N if j != i) {
        for ( k <-  j+1 until N if k != i) {

          val p4 = (points(j)(0)+ points(k)(0) - points(i)(0),points(j)(1) + points(k)(1) - points(i)(1))

          if (pointSet.contains(p4)) {
            val dot = (points(j)(0) - points(i)(0)) * (points(k)(0) - points(i)(0)) + (points(j)(1) - points(i)(1)) * (points(k)(1) - points(i)(1))
            if (dot == 0) ans = ans min area(i,j,k,points)
          }
        }
      }
    }

    if(ans < Double.MaxValue) ans else 0
  }


  def buildVPMap(points: Array[Array[Int]]): mutable.HashMap[(Int,Int), List[(Int, Int)]] = {
    val ans = mutable.HashMap[(Int, Int), List[(Int, Int)]]()
    for(i <- points.indices){
      for(j <- i+1 until points.length){
        val x = points(i)(0) - points(j)(0)
        val y = points(i)(1) - points(j)(1)
        if(x < 0 ){
          ans.put((-x, -y), (i, j)::ans.getOrElse((-x, -y), List[(Int, Int)]()))
        }
        else if(x > 0){
          ans.put((x, y), (i, j)::ans.getOrElse((x, y), List[(Int, Int)]()))
        }else{
          ans.put((0, -y max y), (i, j)::ans.getOrElse((0, -y max y), List[(Int, Int)]()))
        }
      }
    }
    ans
  }
  def isVertical(key1: (Int, Int), key2: (Int, Int)):Boolean = key1._1 * key2._1 + key1._2 * key2._2 == 0
  def isRec(p1: (Int, Int), p2: (Int, Int), l: List[(Int, Int)]): Boolean = {


    val x1 = if(p1._1 < p2._1) (p1._1, p2._1) else (p2._1, p1._1)
    val y1 = if(p1._2 < p2._2) (p1._2, p2._2) else (p2._2, p1._2)
    val x2 = if(p1._1 < p2._2) (p1._1, p2._2) else (p2._2, p1._1)
    val y2 = if(p1._2 < p2._1) (p1._2, p2._1) else (p2._1, p1._2)
    val ans = (l.contains(x1) && l.contains(y1)) ||
      (l.contains(x2) && l.contains(y2))
    if(!ans){
      println(p1, p2, " is not a rectangle")
    }
    ans

  }
  def lenOfVector(v: (Int, Int)): Double = Math.sqrt(v._1 * v._1 + v._2 * v._2)
  def minAreaFreeRect2(points: Array[Array[Int]]): Double = {
    val vpm =buildVPMap(points)
    println(s"Map: $vpm")
    println("-------------")
    var ans = Double.MaxValue
    val keys =  vpm.keySet.toList
    for(i <- keys.indices ){
      for(j <- i+1 until keys.length if isVertical(keys(i), keys(j))){
        println(keys(i),keys(j))
        val pi = vpm(keys(i))
        for(l <- pi.indices){
          for(k <- l+1 until pi.length if isRec(pi(l), pi(k), vpm(keys(j)))){

            //val tmp = lenOfVector(keys(i)) * lenOfVector(keys(j))

            ans = ans min lenOfVector(keys(i)) * lenOfVector(keys(j))
          }
        }
      }
    }
    if(ans == Double.MaxValue) 0 else ans
  }

  def test2(): Unit = {
    val points = Array(
      Array(0,1),
      Array(1,0),
      Array(3,2),
      Array(2,3),
      Array(0,3),
      Array(1,1),
      Array(3,3),
      Array(0,2)
    )
    println(minAreaFreeRect(points))
  }
  def test():Unit = {
    val a = 1
    val b = 1
    println(lenOfVector((a, b)))

    println(4 min 2 * 3)
  }

  def test1(): Unit = {
    val points = Array(

      Array(1,2),
      Array(2,1),
      Array(1,0),
      Array(0,1)
    )
    println(minAreaFreeRect(points))


    val points2 = Array(

      Array(1,2),
      Array(3,1),
      Array(1,3),
      Array(2,1),
      Array(0,3)
    )
    println(minAreaFreeRect(points2))

  }

  def test3():Unit = {
    val p1 = (594,1080)
    val p2 = (1800,-990)
    println(isVertical(p1,p2))
  }
  def main(args: Array[String]): Unit = {
    test2()

  }
}
