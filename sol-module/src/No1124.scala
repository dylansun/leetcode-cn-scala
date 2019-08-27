/**
  * Longest Well-Performing Interval
  */
object No1124 {
  object Solution {
    def longestWPI(hours: Array[Int]): Int = {
      val A = hours.map(x => if(x > 8) 1 else -1)
      val B = Array.fill(A.length)(0)
      B(0) = A(0)
      var max_len = 0
      A.indices.tail foreach {i => B(i) = B(i-1) +A(i)}
      B.indices foreach {i => if(B(i) > 0) max_len = i+1}
      for{
        i <- B.indices
        j <- i+1 until B.length
        if B(j) - B(i) > 0
      } max_len = max_len max (j-i)
      max_len
    }
  }
}
