/**
  * 658. 找到 K 个最接近的元素
  */
object No658 {
  object Solution {
    def findClosestElements(A: Array[Int], k: Int, t: Int): List[Int] = {
      A.sortBy{x => (Math.abs(x - t), x)}.take(k).toList.sorted
    }
  }
}
