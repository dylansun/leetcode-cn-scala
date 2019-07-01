/**
  * Created by lilisun on 5/3/19.
  */
object No799 {
  def champagneTower(n: Int, row: Int, col: Int): Double = {
    val A = Array.fill(row+1,row+1)(0.0)
    A(0)(0) = n
    for{
      i <- 1 to row
      j <- 0 to i
    } j match {
      case 0 => A(i)(j) = (A(i-1)(0) - 1) / 2 max 0
      case x:Int if x == i => A(i)(j) = (A(i-1)(j-1) - 1)/2 max 0
      case _ => A(i)(j) = ((A(i-1)(j) - 1)/2  max 0)+  ((A(i-1)(j-1) - 1)/2 max 0)
    }
    A.foreach(x => println(x.toList))
    A(row)(col) min 1
  }
  def test():Unit = {
    val n = 199
    val row = 23
    val col = 23
    println(champagneTower(n, row, col))
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
