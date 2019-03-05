
object No1004 {

  def longestOnes(nums: Array[Int], k: Int): Int = {
    var res = 0
    var zero = 0
    var left = 0
    for(right <- nums.indices ){
      if(nums(right) == 0) zero +=1
      while(zero > k){
        if(nums(left) == 0) zero -= 1
        left +=1
      }
      res = res max right-left +1
    }
    res
  }
}
