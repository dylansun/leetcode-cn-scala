/**
  * Created by lilisun on 3/9/19.
  */
object No812 {
  def area(i: Int, j: Int, k: Int, points: Array[Array[Int]]): Double = {
    Math.abs(points(i)(0) * points(j)(1) +
              points(k)(1) * points(j)(0) +
              points(k)(0) * points(i)(1) -
              points(i)(0) * points(k)(1) -
              points(j)(0) * points(i)(1) -
              points(k)(0) * points(j)(1)) * 0.5
  }
  def largestTriangleArea(points: Array[Array[Int]]): Double = {
    var ans = 0.0
    val n = points.length
    for( i <- 0 until n)
      for(j <- i + 1 until n)
        for(k <- j + 1 until n){
          ans = ans max area(i,j,k, points)
        }
    ans
  }
}
