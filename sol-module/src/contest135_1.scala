/**
  * Created by lilisun on 5/5/19.
  */
object contest135_1 {

  def isBoomerang(points: Array[Array[Int]]): Boolean = {
    val p1 = points(0)
    val p2 = points(1)
    val p3 = points(2)
    if(p1(0) == p2(0)) {
      if(p3(0) == p1(0)) return false
      else true
    }
    else{
      val k = (p1(1)- p2(1)).toDouble / (p1(0) - p2(0)).toDouble
      val b = p1(1) - p1(0)*k
      if(Math.abs(p3(0) * k  + b - p3(1)) < 1e-9)  false
      else true
    }
  }
  def main(args: Array[String]): Unit = {

  }
}
