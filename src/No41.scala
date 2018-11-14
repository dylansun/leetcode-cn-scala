object No41 {
  def firstMissingPositive(nums: Array[Int]): Int = {
    if(nums.isEmpty) return 1
    (1 to ((nums.max max 0) +1)).toArray.dropWhile({nums.contains(_)}).min
  }

  def main(args: Array[String]): Unit = {
    val nums = Array[Int]()
    println(firstMissingPositive(nums))
  }
}
