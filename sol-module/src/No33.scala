/**
  * Created by lilisun on 2/23/19.
  */
object No33 {
  def search(nums: Array[Int], target: Int): Int = {

    @annotation.tailrec
    def search(lo: Int, hi: Int): Int =
      if (lo >= hi) if(nums(lo) == target) lo else -1
      else {
        val m = lo + (hi - lo) / 2
        if (nums(m) == target) m
        else {
          if (nums(lo) < nums(hi)) if (nums(m) < target) search(m + 1, hi) else search(lo, m - 1)
          else if (nums(lo) == target) lo else search(lo + 1, hi)
        }

      }

    if (nums.isEmpty) -1 else search(0, nums.length - 1)
  }
}
