/**
  * Created by lilisun on 3/10/19.
  */
object No724 {
  def pivotIndex(nums: Array[Int]): Int = {

    val cum = cumSum(nums)
    nums
      .indices
      .find( x =>{ cum(x) == cum.last - cum(x) + nums(x)
      })
      .getOrElse(-1)
  }

  def cumSum(nums: Array[Int]):Array[Int] = {
    val cum = Array.fill(nums.length)(0)
    for(x <- nums.indices) x match {
      case 0 => cum(0) = nums(0)
      case _ => cum(x) = cum(x-1) + nums(x)
    }
    cum
  }

  def pivotIndex2(nums: Array[Int]): Int = {

    nums
      .indices
      .find( x =>{
        nums.slice(0, x).sum == nums.slice(x+1, nums.length).sum
      })
      .getOrElse(-1)
  }

  def main(args: Array[String]): Unit = {
    val A = Array(1, 7, 3, 6, 5, 6)
    println(pivotIndex(A))
  }
}
