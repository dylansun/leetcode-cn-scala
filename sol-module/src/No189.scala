/**
  * Created by lilisun on 2/25/19.
  */
object No189 {
  def rotate(nums: Array[Int], k: Int): Unit = {
    var i = k % nums.length
    while(i > 0){
      val t = nums.last
      for(j <- (1 until nums.length).reverse) nums(j) = nums(j-1)
      nums(0) = t
      i -= 1
    }
  }


  def main(args: Array[String]): Unit = {
    var nums = Array(0,1,2,3)
    rotate(nums, 2)
    println(nums.mkString)

  }
}
