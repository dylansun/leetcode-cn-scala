/**
  * Grumpy Bookstore Owner
  */
object No1052 {
  object Solution {
    def maxSatisfied(A: Array[Int], B: Array[Int], X: Int): Int = {
      val ori = (A zip B).map {case (x,y) => x * (1-y)}.sum
      def slide_window(i:Int,init:Int,  acc:Int):Int = {
        if (i + X == A.length) acc
        else {
          val n_window = init  - A(i)*B(i) + A(i + X) * B(i + X)
          slide_window(i + 1, n_window, n_window max acc )
        }
      }
      val init = (A.slice(0, X) zip B.slice(0, X)).map {case (x, y) => x * y }.reduce(_+_)
      ori + slide_window(0, init, init)
    }
  }
}
