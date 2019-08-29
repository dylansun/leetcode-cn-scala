/**
  * Created by lilisun on 8/29/19.
  */
object No373 {
  object Solution {
    def kSmallestPairs(A: Array[Int], B: Array[Int], k: Int): List[List[Int]] = {
      (for {
        x <- A
        y <- B
      } yield List(x,y)).sortBy{case x::y::Nil => x + y}.toList.take(k)
    }
  }
}
