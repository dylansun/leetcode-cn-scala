object No637 {
  def averageOfLevels(root: TreeNode): Array[Double] = {
    @annotation.tailrec
    def averageOfLevels(curLevel: List[TreeNode], res: List[Double]): Array[Double] =
      if (curLevel.isEmpty)
        res.reverse.toArray
      else {
        val (nextLevel, len) = curLevel.foldLeft((List.empty[TreeNode], 0)) { case ((ls, count), node) =>
          if (node.left != null && node.right != null)
            (node.left :: node.right :: ls, count + 1)
          else if (node.left != null)
            (node.left :: ls, count + 1)
          else if (node.right != null)
            (node.right :: ls, count + 1)
          else
            (ls, count + 1)
        }
        averageOfLevels(nextLevel, curLevel.map(_.value.toLong).sum.toDouble / len :: res)
      }

    if (root != null)
      averageOfLevels(List(root), List())
    else
      Array.empty
  }
}
