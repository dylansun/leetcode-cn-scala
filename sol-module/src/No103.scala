object No103 {
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    @annotation.tailrec
    def zigzagLevelOrder(curLevel: List[TreeNode], res: List[List[Int]], isReverse: Boolean): List[List[Int]] = {
      if (curLevel.isEmpty)
        res
      else {
        val nextLevel = curLevel.foldRight(List.empty[TreeNode]) { (node, ls) =>
          if (node.left != null && node.right != null)
            node.left :: node.right :: ls
          else if (node.left != null)
            node.left :: ls
          else if (node.right != null)
            node.right :: ls
          else
            ls
        }
        if (isReverse)
          zigzagLevelOrder(nextLevel, curLevel.map(_.value).reverse :: res, !isReverse)
        else
          zigzagLevelOrder(nextLevel, curLevel.map(_.value) :: res, !isReverse)
      }
    }

    if (root != null)
      zigzagLevelOrder(List(root), List(), false).reverse
    else
      Nil
  }
}
