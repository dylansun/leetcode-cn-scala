object No153 {
  def findMin(nums: Array[Int]): Int = {

    @annotation.tailrec
    def findMin(lo: Int, hi: Int): Int =
      if (lo >= hi) nums(lo)
      else {
        val m = lo + (hi - lo) / 2
        if (nums(m) < nums(hi)) findMin(lo, m)
        else findMin(m + 1, hi)
      }

    findMin(0, nums.length - 1)
  }
}
