/**
  * Created by lilisun on 1/10/19.
  */
object No27 {
  def removeElement(nums: Array[Int], value: Int): Int = {
    var i = 0
    for (j <- 0 to nums.length -1) {
      if (nums(j) != value) {
        nums(i) = nums(j)
        i =  i + 1
      }
    }
    return i
  }
}
