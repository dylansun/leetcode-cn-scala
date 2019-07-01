/**
  * Created by lilisun on 6/27/19.
  */
object No276 {

  // n = 3, k = 2
  // f 1 -> 2
  // f 2 -> f 1 * 1 = 2, g 2 -> f 1 * 1 = 2
  // f 3 -> g 2 * (k - 1) + f 2 * (k-1) , g 3 -> f 2 * 1 = 2
  //
  // flag:是否和前一种颜色相同, 0 agree, 1 disagree with previous color
  // dp 0, 0: k, dp 0,1: ?
  // dp 1, 0: k  or 1
  def numWays(n: Int, k: Int): Int = {
    val A = Array.fill(n)(0)
    val B = Array.fill(n)(0)
    A(0) = k
    B(0) = 0
    for{i <- 1 until n}{
      A(i) = (A(i-1) + B(i-1)) * (k -1)
      B(i) = A(i-1)
    }
    A.last + B.last
  }
  def numWaysF(n: Int, k: Int): Int = {
    def f(a:Int, b:Int, n:Int):Int = {
      if(n == 0) a + b
      else f((k-1)*(a + b), a, n-1)
    }
    if(n == 0 || k == 0) 0
    else f(k, 0, n-1)
  }


  def main(args: Array[String]): Unit = {
    println(numWaysF(8,4))
  }
}
