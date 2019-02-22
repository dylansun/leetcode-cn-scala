object No128 {
  def longestConsecutive(nums: Array[Int]): Int = if (nums.isEmpty) 0
  else {
    val len = nums.length
    val parent = (0 until len).toArray
    val size = Array.fill(len)(1)

    @annotation.tailrec
    def root(p: Int): Int =
      if (p != parent(p)) {
        parent(p) = parent(parent(p))
        root(parent(p))
      } else p

    def union(par: Int, child: Int): Unit = {
      val rootParent = root(par)
      val rootChild = root(child)
      parent(rootChild) = rootParent
      size(rootParent) += size(rootChild)
    }


    nums.zipWithIndex.foldLeft(Map[Int, Int]()) { case (map, (num, i)) =>
      if (map.contains(num)) map
      else {
        if (map.contains(num - 1)) {
          val childIndex = map(num - 1)
          union(i, childIndex)
        }
        if (map.contains(num + 1)) {
          val parentIndex = map(num + 1)
          union(parentIndex, i)
        }
        map + (num -> i)
      }
    }
    size.max
  }
}
