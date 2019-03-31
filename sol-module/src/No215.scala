/**
  * Created by lilisun on 3/28/19.
  */
import scala.collection.mutable
object No215 {
  // priority queue, pq holds the largest k number, a0 .. ak-1, a0 <= ak-1
  // f(pq)(x) => new_pq , insert x to pq , x < a0 do nothing, else remove a0 insert x to pq
  def f(pq: mutable.PriorityQueue[Int])(x:Int): mutable.PriorityQueue[Int] = {
    pq.enqueue(pq.dequeue max x)
    pq
  }
  def init(pq: mutable.PriorityQueue[Int], nums: Array[Int]):mutable.PriorityQueue[Int] = {
    nums.foreach(x => pq.enqueue(x))
    pq
  }
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    // use first k to construct pq
    nums.slice(k, nums.length).foldLeft(init(mutable.PriorityQueue[Int]()(Ordering.by(x => -x)),nums.slice(0,k)))( (pq, x) => f(pq)(x)).dequeue
  }

  def main(args: Array[String]): Unit = {
    val A = Array(3,2,1,4,8,9,10,7)
    println(findKthLargest(A,6))
  }
}
