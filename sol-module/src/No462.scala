/**
  * 462. 最少移动次数使数组元素相等 II
  */
object No462 {
  object Solution {
    def minMoves2(A: Array[Int]): Int = {
      val B = A.sorted
      val index = B.length / 2
      B.map { x => Math.abs(x - B(index))}.sum
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
