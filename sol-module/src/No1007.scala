/**
  * Created by lilisun on 3/10/19.
  */
object No1007 {
  def minDominoRotations(A: Array[Int], B: Array[Int]): Int = {
    val n = (A zip B)
      .map( x=> Set(x._1, x._2))
      .reduce(_ intersect _ )
    if(n.isEmpty) -1
    else{
      (A.length - A.count(_ == n.head)) min (B.length - B.count(_==n.head))
    }
  }
}
