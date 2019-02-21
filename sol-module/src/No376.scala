/**
  * Created by lilisun on 2/22/19.
  */
object No376 {
  def wiggleMaxLength(nums: Array[Int]): Int = {
    if(nums.length <= 1) return nums.length
    if(nums(0) == nums(1)) return wiggleMaxLength(nums.tail)
    if(nums.length == 2) return 2
    var flag = if(nums(0) > nums(1)) true else false
    var right = nums(1); var ans = 2
    for(x <- 2 to nums.indices.last){
      if(right != nums(x) && flag == (right < nums(x))){
          flag = !flag
          ans += 1
        }
      right = nums(x)
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val t = Array(0,0,0,1,0,1,2,3,2)
    println(wiggleMaxLength(t))
  }
}
