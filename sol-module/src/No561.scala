
object No561 {
  def arrayPairSum(nums: Array[Int]): Int =
    nums.sorted.zipWithIndex.withFilter(_._2 % 2 == 0).map(_._1).sum
}
