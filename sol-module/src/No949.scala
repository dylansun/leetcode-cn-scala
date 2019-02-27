/**
  * Created by lilisun on 2/27/19.
  */
object No949 {
  def largestTimeFromDigits(A: Array[Int]): String = {
    def f(A: Array[Int]): Boolean = A(0)*10 + A(1) < 24 && A(2) * 10 + A(3) < 60
    def t(A: Array[Int]): (Int, Int) = (A(0)*10 + A(1), A(2) * 10 + A(3))
    // if we have different cc, make cc a parameter
    def c(x: (Int, Int), cc: Int => String): String = cc(x._1) + ":" + cc(x._2)
    def c2(x: (Int, Int)): String = c(x, cc)
    def cc(x:Int): String = if(x<10) "0" + x.toString else x.toString

    A.permutations.toList filter f  match {
      case x:List[Array[Int]] if x.isEmpty => ""
      case x:List[Array[Int]] if x.nonEmpty  => c2((x map t).max)
    }
  }

  def main(args: Array[String]): Unit = {
    val A = Array(2,3,1,2)
    def k(x: Int):Int = x -1
    println((A map k).mkString)
    println(largestTimeFromDigits(A))
  }
}
