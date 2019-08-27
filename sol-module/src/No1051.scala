/**
  * Height Checker
  */
object No1051 {
  object Solution {
    def heightChecker(heights: Array[Int]): Int = {
      (heights zip heights.sorted).count{case (x,y) => x != y}
    }
  }
}
