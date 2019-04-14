/**
  * Created by lilisun on 1/15/19.
  */
import scala.language.postfixOps
object No886 {
  def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
    val adj = Array.fill(N+1, N+1)(false)
    for(x <- dislikes) {
      adj(x(0))(x(1)) = true
      adj(x(1))(x(0)) = true
    }

    val col = Array.fill(1+N)(0)
    col(0) = Int.MaxValue
    f(adj, col, N)
  }

  def f(graph:Array[Array[Boolean]], col:Array[Int], n:Int):Boolean = {
    if(!col.contains(0)) true else {
      var queue = List(col.indexOf(0))
      var color = 1
      while(queue.nonEmpty){
        queue.foreach{x => col(x) match{
          case 0 => col(x) = color
          case preColor if preColor == color =>
            println("This should not happen!")
          case _ => return false
        }
        }
        color = {if(color == 1) 2 else 1}
        queue = queue flatMap g(n,graph) filter h(color, col) distinct
      }
      f(graph, col, n)
    }
  }

  def g(n:Int, graph:Array[Array[Boolean]])(x:Int):IndexedSeq[Int] = (1 to n) filter graph(x)
  def h(color:Int, col:Array[Int])(x:Int):Boolean = col(x) != color
  def ps(s: String, n: Int, d: Double): String = {
    s"first: $s, second: $d, third: $d"
  }
  def main(args: Array[String]): Unit = {
    val t = Vector(1, 2, 5, 6)
    val s  = t.reduce((sum, n) => sum + n -1)
    val m = t.reduce((sum, n) => sum * n + 1 )
    println(s"sum of t is $s")
    println(s"mutiply of t is $m")

    val mt = t.map(x => x * 11 -1 )
    println(s"map of t is $mt")

    var sum = 0
    val ft = t.foreach(x => sum += x * 11 -1)
    println(s"foreach of t is $ft")
    println(s"sum is $sum")

    val for_t = (for(x <- t) yield x*11 -1)
    println(s"for loop of t is $for_t")
    println(ps("hello", 1, 3.14))
  }
}
