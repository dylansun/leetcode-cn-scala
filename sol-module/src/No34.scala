/**
  * Created by lilisun on 11/13/18.
  */
object No34 {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    /**
      * (l, r): array[l] = target & array[l -1] < target
      *
      */
    return Array(nums.indexOf(target),nums.lastIndexOf(target))
  }
}
