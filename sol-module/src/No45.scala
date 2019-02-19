/**
  * Created by lilisun on 2/19/19.
  */
object No45 {
  def jump(nums: Array[Int]): Int = jh(nums, 0, 0)
  def jh(nums: Array[Int], pos: Int, acc: Int): Int = {
    if(pos >= nums.length -1) return acc
    if(pos + nums(pos) >= nums.length -1) return acc + 1
    val tp = (for(x <- pos+1 to pos + nums(pos)) yield (x , nums(x) + x)).toList
    jh(nums, tp.maxBy(_._2)._1, acc + 1)
  }
  def main(args: Array[String]): Unit = {
    val a = Array(1,2,3,4,5,6,7,8,9)
    println(jump(a))
  }
}
