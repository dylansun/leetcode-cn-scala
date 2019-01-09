/**
  * Created by lilisun on 1/10/19.
  */
object No31 {
  def nextPermutation(nums: Array[Int]): Unit = {

    var i = nums.length - 2
    while (i >= 0 && nums(i + 1)<= nums(i)) {
      i = i - 1
    }
    if (i >= 0) {
      var j = nums.length - 1
      while (j >= 0 && nums(j) <= nums(i)) {
        j = j -1
      }
      swap(nums, i, j)
    }
    reverse(nums, i + 1)
  }

  def swap(nums: Array[Int], i: Int, j: Int): Unit = {
    val temp = nums(i)
    nums(i) = nums(j)
    nums(j) = temp
  }

  def reverse(nums: Array[Int], start: Int) : Unit = {
    var i = start
    var j = nums.length - 1
    while (i < j) {
      swap(nums, i, j)
      i = i + 1
      j = j - 1
    }
  }
}
