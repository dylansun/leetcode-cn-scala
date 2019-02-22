
object No108 {
  def sortedArrayToBST(nums: Array[Int]): TreeNode =
    if (nums.isEmpty) null
    else {
      val len = nums.length
      val v = nums(len /2 )
      val res = new TreeNode(v)
      val (l, r) = nums.zipWithIndex.partition(_._2 < len / 2)
      res.left = sortedArrayToBST(l.unzip._1)
      res.right = sortedArrayToBST(r.tail.unzip._1)
      res
    }
}
