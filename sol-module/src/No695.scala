/**
  * Created by lilisun on 5/9/19.
  */
object No695 {
  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val n = grid.length; if(n == 0) return 0
    val m = grid(0).length
    val visited = Array.fill(n, m)(false)
    var ans = 0
    def f(p:(Int,Int)):List[(Int,Int)] = p match {
      case (i,j) => List((i+1, j), (i-1,j),(i,j+1),(i,j-1))
    }
    def inBound(p:(Int, Int)):Boolean = p match{
      case (i,j) =>
        i>= 0 &&
          j>= 0 &&
          i < n &&
          j < m
    }
    def dfs(l:List[(Int,Int)], acc:Int):Int = l match {
      case Nil => acc
      case _ =>
        l.foreach{case (i,j) => visited(i)(j) = true}
        dfs((l.view flatMap f filter inBound filter {case (i,j) => !visited(i)(j) && grid(i)(j) == 1}).distinct.force.toList, acc + l.length)
    }
    for{
      i <- 0 until n
      j <- 0 until m
    }{
      if(grid(i)(j) == 1 && !visited(i)(j)){
        ans = ans max dfs((i,j)::Nil,0)
      }
    }
    ans
  }
  def g: (Int,Int) => Boolean =  {case (i,j) => i > 1 && j > 1}

  def main(args: Array[String]): Unit = {
    val a:(Int, Int) = (3,2)
    println(g(2,2))
  }
}

