/**
  * Created by lilisun on 2/25/19.
  */
object No219 {
  def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean = k match{
    case 0 => false
    case x:Int if nums.length == 1 => false
    case x:Int if x >= nums.length => nums.distinct.length == nums.length
    case _ => (0 until nums.length - k).foldLeft(false)((f, x) => nums.slice(x, x+k+1).distinct.size == k || f)
    //x = 0, k =2; 0 1 2
  }
  def main(args: Array[String]): Unit = {
    val a = Array(1,2,3, 1,2,3)
    println(containsNearbyDuplicate(a, 2))
    val b = Array[Int]()
    println(b.distinct.length)
  }
}
