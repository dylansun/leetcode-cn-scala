
object No747 {
  def dominantIndex(nums: Array[Int]): Int = {
    var largestValueIndex = -1
    val (firstLargest, secondLargest) = nums.zipWithIndex.foldLeft((Int.MinValue, Int.MinValue)) {
      case ((fst, snd), (num, idx)) =>
        if (num > fst) {
          largestValueIndex = idx
          (num, snd max fst)
        } else if (num > snd)
          (fst, num)
        else (fst, snd)
    }
    if (firstLargest >= 2 * secondLargest) largestValueIndex else -1
  }
}
