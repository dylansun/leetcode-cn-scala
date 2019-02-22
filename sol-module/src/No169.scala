object No169 {
  def majorityElement(nums: Array[Int]): Int = {
    var res = 0
    nums.foldLeft(0) { (count, num) =>
      if (count == 0) {
        res = num
        1
      } else if (num == res)
        count + 1
      else
        count - 1
    }
    res
  }
}