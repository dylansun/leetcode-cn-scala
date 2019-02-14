/**
  * Created by lilisun on 1/15/19.
  */
object No886 {
  def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
true
  }

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
