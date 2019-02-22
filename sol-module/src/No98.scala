object No98 {
  def isValidBST(root: TreeNode): Boolean = {

    @annotation.tailrec
    def sweepLeft(root: TreeNode, stack: List[TreeNode]): List[TreeNode] =
      if (root == null) stack
      else sweepLeft(root.left, root :: stack)

    val stack = sweepLeft(root, List.empty)

    @annotation.tailrec
    def inorderTraverse(prev: Option[Int], stack: List[TreeNode]): Boolean =
      if (stack.isEmpty) true
      else stack match {
        case h :: t =>
          val s = if (h.right != null) sweepLeft(h.right, t) else t
          if (prev.isDefined && prev.get >= h.value) false
          else inorderTraverse(Some(h.value), s)
      }

    inorderTraverse(None, stack)
  }
}
