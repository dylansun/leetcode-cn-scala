
object No268 {
  def missingNumber(nums: Array[Int]): Int = {
    val t = (0 to nums.length).foldLeft(0)(_ ^ _)
    nums.foldLeft(t)(_ ^ _)
  }
}
