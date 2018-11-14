/**
  * Created by lilisun on 11/13/18.
  */
object No35 {
  def searchInsert(nums: Array[Int], target: Int): Int = {
    if(target < nums(0)) return 0
    if(target > nums.last) return nums.length

    val idx = nums.indexOf(target)
    if(idx == -1) return nums.indexWhere({x:Int => x > target})


    return idx

  }
}
