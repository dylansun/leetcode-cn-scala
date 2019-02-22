object No532 {
  def findPairs(nums: Array[Int], k: Int): Int = if (k < 0) 0 else {
    var res = 0
    val (m, s) = nums.foldLeft((Map.empty[Int, Int], Set.empty[Int])) { case ((map, set), num) =>
      (map.updated(num, map.getOrElse(num, 0) + 1), set + num)
    }
    for (num <- s) {
      if (k != 0 && s.contains(num + k))
        res += 1
      else if (k == 0 && m(num) > 1)
        res += 1
    }
    res
  }
}
