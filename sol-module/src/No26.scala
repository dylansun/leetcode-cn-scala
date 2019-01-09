/**
  * Created by lilisun on 1/10/19.
  */
object No26 {
  def removeDuplicates(nums: Array[Int]): Int = {
    if (nums.length == 0) return 0
    var i = 0
    for ( j <- 1 to  nums.length-1) {
      if (nums(j) != nums(i)) {
        i = i + 1
        nums(i) = nums(j)
      }
    }
    return i + 1
  }
}
