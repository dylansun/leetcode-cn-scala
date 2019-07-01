/**
  * Created by lilisun on 5/14/19.
  */
object No442 {
  object SwapIndexSolution {
    def findDuplicates(nums: Array[Int]): List[Int] = {
      for{i <- 0 until nums.length}{
        while((i+1) != nums(i) && nums(nums(i) - 1) != nums(i)){
          val tmp = nums(i)
          nums(i) = nums(nums(i) - 1)
          nums(tmp - 1) = tmp
        }
      }
      nums.indices.toList filter {i => nums(i) != i+1} map nums
    }
  }
  object  Solution{
    def findDuplicates(nums:Array[Int]):List[Int] = {
      var ans = List.empty[Int]
      for{i <- nums.indices}{
        if(nums(Math.abs(nums(i)) - 1 ) < 0) ans ::= Math.abs(nums(i))
        nums(Math.abs(nums(i)) - 1 ) *= -1
        println(nums.toList)
      }
      ans
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.findDuplicates(Array(4,3,2,7,8,2,3,1)))
  }
}
