object No40 {
  def permute(nums: Array[Int]): List[List[Int]] = {
      nums.toList.permutations.toList
  }

  def main(args: Array[String]): Unit = {
    val a = Array(1,2,3)
    println(permute(a).mkString)
  }
}
