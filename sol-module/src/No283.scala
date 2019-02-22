
object No283 {
  def moveZeroes(nums: Array[Int]): Unit = {
    val index = nums.foldLeft(0) {
      (i, num) =>
        if (num != 0) {
          nums(i) = num
          i + 1
        } else
          i
    }
    (index until nums.length).foreach(nums(_) = 0)
  }
}