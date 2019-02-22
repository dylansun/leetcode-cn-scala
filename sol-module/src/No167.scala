object No167 {
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    @annotation.tailrec
    def twoSum(lo: Int, hi: Int): Array[Int] = {
      val s = numbers(lo) + numbers(hi)
      if (s == target)
        Array(lo + 1, hi + 1)
      else if (s < target)
        twoSum(lo + 1, hi)
      else //
        twoSum(lo, hi - 1)
    }

    twoSum(0, numbers.length - 1)
  }
}
