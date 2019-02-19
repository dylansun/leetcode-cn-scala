/**
  * Created by lilisun on 2/19/19.
  */

import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits._
object No502 {
  def findMaximizedCapital(k: Int, W: Int, Profits: Array[Int], Capital: Array[Int]): Int = {
    fmcbypq(k, W, Profits, Capital)
  }

  def fmc(k: Int, W: Int, Profits: Array[Int], Capital: Array[Int], ans: Int): Int = {
    if(k == 0) return ans
    val candi = for(x <- Profits.indices if Capital(x) <= W) yield (x, Profits(x))
    if(candi.isEmpty) return ans
    println(s"candi size: ${candi.size}")
    val gain = candi.reduce((x, y) => if(x._2 > y._2) x else y)
    val prof = Profits
    prof(gain._1) = 0
    fmc(k -1, W + gain._2, prof, Capital, gain._2 + ans)
  }

  def fmcbypq(k:Int, w:Int, p: Array[Int], c:Array[Int]): Int = {
    if(k == 0) return w
    val profpq = new PriorityQueue[(Int,Int)]()(Ordering.by(x =>   x._1))
    val cappq = new PriorityQueue[(Int, Int)]()(Ordering.by(x => - x._2))
    for(x <- c.indices){
      if(c(x) > w) cappq.enqueue(p(x) -> c(x))
      else profpq.enqueue(p(x) -> c(x))
    }

    println(s"profit pq length: ${profpq.length}")
    println(s"canpital pq length: ${cappq.length}")
    println(s"cap pq: ${cappq.mkString}")
    var ans = w
    for(x <- 0 until k){
      println(s"x: $x, k: $k, profit pq length: ${profpq.length}")
      if(profpq.isEmpty) return ans
      ans += profpq.dequeue()._1
      while(cappq.nonEmpty && ans >= cappq.head._2){
        profpq.enqueue(cappq.dequeue())
      }
    }
    ans
  }


  def main(args: Array[String]): Unit = {
    val profits = Array(1,2,3)
    val capitals = Array(0,1,1)
    println(findMaximizedCapital(2,0, profits, capitals))

  }
}
