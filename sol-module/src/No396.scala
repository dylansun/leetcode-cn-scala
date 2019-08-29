object No396 {
  object Solution {
    def maxRotateFunction(A: Array[Int]): Int = {
      if(A.isEmpty) return 0
      val F = Array.fill(A.length)(0)
      F(0) = A.indices map {i => i * A(i)} sum
      val s = A.sum
      val n = A.length
      F.indices.tail foreach {i => F(i) = F(i-1) + s - n * A(n- i) }
      F.max
    }
  }
}
