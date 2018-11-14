
object No665 {
  def checkPossibility(nums: Array[Int]): Boolean = {
    for(i <- 0 to (nums.length - 2)){
      /**if (nums(i) > nums(i+1)){
        val temp = nums(i + 1)
        nums(i+1) = nums(i)
        if(nums.equals(nums.sorted))
      }**/
    }
    false

  }
  def checkPossibility2(nums: Array[Int]): Boolean = {
    var tol = true
      for(i <- 0 to nums.length - 2 if nums(i) > nums(i + 1)){
        if(tol){
          if(i == 0)
            nums(i) = nums(i + 1)
          else if(i == nums.length -2)
            nums(i + 1) = nums(i)
          else{
            if( nums(i-1) <= nums(i) && nums(i) <= nums(i+2))
              nums(i+1) = nums(i)
            else if( nums(i-1) <= nums(i+1) && nums(i+1) <= nums(i+2))
              nums(i) = nums(i+1)
            else
              return false
          }
          tol = false

        }
        else{
          return false
        }
    }
    return true
  }

  def main(args: Array[String]): Unit = {
/**    a > b >c
    a >b  c > d **/
    val s = Array(2,3,3,2,4)
    val res = checkPossibility(s)
    println(res)
  }
}
