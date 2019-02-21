/**
  * Created by lilisun on 2/22/19.
  */
object No55 {
  def canJump(nums: Array[Int]): Boolean = {
    var pos = 0
    while(true){
      println(s"position: $pos")
      if(pos >= nums.indices.last || pos + nums(pos) >= nums.indices.last) return true
      val next = (0 to pos + nums(pos)).map(x => x + nums(x)).max
      if(next == pos) return false
      pos = next
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(5,9,3,2,1,0,2,3,3,1,0,0)
    println(canJump(nums))
  }
}
