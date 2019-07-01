/**
  * Created by lilisun on 5/10/19.
  */
object No576 {
  object Solution {
    def findPaths(m: Int, n: Int, N: Int, i: Int, j: Int): Int = {
      if(!inBound(m,n)((i,j)) || N == 0) return 0
      val mod = (1e9+7).toInt
      val dp = Array.fill(m,n, N)(0)
      dp(i)(j)(0) = 1
      for{k <- 1 until N}{
        for{
          i <- 0 until m
          j <- 0 until n
        }
          dp(i)(j)(k) = (nei((i,j)) filter inBound(m,n) map {case(x,y) => dp(x)(y)(k-1)}).foldLeft (0) {(sum,x) => (x + sum) % mod}
      }


      ((for{
        i <- 0 until m
        j <- 0 until n
        if i == 0 ||j==0 ||i == m-1 || j == n-1
      } yield (i,j)) ++ bonus(m,n) flatMap {case (i,j) => (0 until N).toList map dp(i)(j)}). foldLeft (0) ((sum,x) => (x + sum) % mod)
    }
    def bonus(n:Int, m:Int):List[(Int,Int)] = (n, m) match {
      case (1,1) => List((0,0), (0,0), (0,0))
      case (1,_) => List((0,0), (0, m-1)) ++ (0 until m).toList.map{i => (0,i)}
      case (_,1) => List((0,0), (n-1, 0)) ++ (0 until n).toList.map{i => (i, 0)}
      case (_,_) => List((0,0), (0, m-1), (n-1, 0), (n-1, m-1))

    }
    def nei(p:(Int,Int)):List[(Int,Int)] = p match {
      case (i,j) => List((i+1,j),(i-1,j), (i, j+1), (i,j-1))
    }
    def inBound(n:Int,m:Int)(p:(Int,Int)):Boolean = p match{
      case (i,j) => i >= 0 && j >= 0 && i < n && j < m
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.findPaths(2,3,8,1,0))
  }
}
