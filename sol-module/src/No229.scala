
object No229 {
  def majorityElement(nums: Array[Int]): List[Int] = {
    var num1, num2 = 0
    nums.foldLeft((0, 0)) {
      case ((c1, c2), num)
      => if (c1 != 0 && num == num1)
        (c1 + 1, c2)
      else if (c2 != 0 && num == num2)
        (c1, c2 + 1)
      else if (c1 == 0) {
        num1 = num
        (1, c2)
      }
      else if (c2 == 0) {
        num2 = num
        (c1, 1)
      } else
        (c1 - 1, c2 - 1)
    }
    val (count1, count2) = nums.foldLeft((0, 0)) { case ((c1, c2), num) =>
      if (num == num1)
        (c1 + 1, c2)
      else if (num == num2)
        (c1, c2 + 1)
      else
        (c1, c2)
    }
    if (count1 > nums.length / 3 && count2 > nums.length / 3)
      num1 :: num2 :: Nil
    else if (count1 > nums.length / 3)
      num1 :: Nil
    else if (count2 > nums.length / 3)
      num2 :: Nil
    else
      Nil
  }
}
