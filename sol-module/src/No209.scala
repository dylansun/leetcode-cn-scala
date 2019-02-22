
object No209 {
  def minSubArrayLen(s: Int, nums: Array[Int]): Int = {
    val len = nums.length

    @annotation.tailrec
    def findMinLen(lo: Int, hi: Int, sum: Int, minLen: Int): Int =
      if (hi >= len) {
        if (lo < len && sum - nums(lo) >= s) findMinLen(lo + 1, hi, sum - nums(lo), minLen min (hi - lo))
        else if (minLen != Int.MaxValue || sum >= s) minLen min (hi - lo)
        else 0
      }
      else if (sum < s) findMinLen(lo, hi + 1, sum + nums(hi), minLen)
      else findMinLen(lo + 1, hi, sum - nums(lo), minLen min (hi - lo))

    findMinLen(0, 0, 0, Int.MaxValue)
  }
}
