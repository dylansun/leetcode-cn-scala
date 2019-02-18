/**
  * Created by lilisun on 2/16/19.
  */
object No973 {
  def kClosest(points: Array[Array[Int]], K: Int): Array[Array[Int]] = kClosest1(points, K)
  def kClosest1(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val distance = (for(x <- points) yield x.map(x => x * x ).sum).toList
    val distance_sorted = distance.sorted
    val k_value = distance_sorted(K)
    points.filter( x => x.map(x => x * x).sum < k_value)
  }

  def kClosest2(points: Array[Array[Int]], K: Int): Array[Array[Int]] ={
    val k_value =points.map( x => x.map( x => x * x ).sum).sorted.array(K)
    points.filter( x => x.map(x => x * x).sum < k_value)
  }


  def main(args: Array[String]): Unit = {
    val a = Array(Array(1, 2), Array(2,3), Array(4,4))
    val b = kClosest(a, 1).mkString
    println(b)
  }
}
