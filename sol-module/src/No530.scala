object No530 {
  def getMinimumDifference(root: TreeNode): Int = {
    // use list to simulate stack
    @annotation.tailrec
    def inOrderTraversal(stack: List[TreeNode], prev: Option[TreeNode], res: Int): Int = stack match {
      case head :: s =>
        val minDiff = res min (prev match {
          case Some(r) => (r.value - head.value).abs
          case None => Int.MaxValue
        })
        inOrderTraversal(f(head.right, s), Some(head), minDiff)
      case Nil => res
    }

    @annotation.tailrec
    def f(root: TreeNode, stack: List[TreeNode]): List[TreeNode] = if (root == null) stack
    else f(root.left, root :: stack)

    inOrderTraversal(f(root, List.empty), None, Int.MaxValue)
  }
}
