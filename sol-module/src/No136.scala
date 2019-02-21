/**
  * Created by lilisun on 2/21/19.
  */
object No136 {
  def singleNumber(nums: Array[Int]): Int = {
    var ans = 0
    nums.foreach(x => ans = ans ^ x)
    ans
  }
}
