/**
  * Created by lilisun on 5/1/19.
  */
object No808 {
  val mem = scala.collection.mutable.HashMap[(Int,Int), Double]()
  def soupServings(N: Int): Double = {
    if(N > 5000) 1 else
      f(Math.ceil(N / 25.0).toInt, Math.ceil(N / 25.0).toInt)
  }
  def f(a:Int, b:Int):Double =
    if(mem.keySet.contains((a,b))) mem((a,b))
    else {
      val ans = (a, b) match{
        case (0, 0) => 0.5
        case (0, _) => 1
        case (_, 0) => 0
        case (_, _) =>
          0.25 * f((a - 4) max 0, b) +
            0.25 * f((a - 3) max 0, (b - 1) max 0) +
            0.25 * f((a - 2) max 0, (b - 2) max 0) +
            0.25 * f((a - 1) max 0, (b - 3) max 0)
      }
      mem((a,b)) = ans
      mem((a,b))
    }
  def main(args: Array[String]): Unit = {

  }
}
