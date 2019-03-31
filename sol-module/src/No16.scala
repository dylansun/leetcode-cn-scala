/**
  * Created by lilisun on 3/29/19.
  */
object No16 {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    target + (0 until nums.length - 2).map(minDist(nums.sorted,target)).reduce(g(0))
  }
  def minDist(nums:Array[Int], target:Int)(i:Int):Int = {
    f(nums,target - nums(i))(i+1, nums.length - 1, nums(i+1) + nums.last + nums(i) - target)
  }
  // find min distance (lo, high) to target
  def f(nums:Array[Int], target:Int)(lo:Int, hi:Int, acc:Int):Int = lo < hi match {
    case false => acc
    case _ => nums(lo) + nums(hi) - target  match {
      case 0 => 0
      case x:Int if x < 0 => f(nums, target)(lo+1, hi, g(target)(nums(lo) + nums(hi), target +acc))
      case _ => f(nums, target)(lo, hi -1, g(target)(nums(lo)+ nums(hi), target + acc))
    }
  }
  def g(target:Int)(x:Int, y:Int):Int = {
    if(Math.abs(x - target) < Math.abs(y - target)) x - target else y - target
  }
}
