
object No94 {
  def inorderTraversal(root: TreeNode): List[Int] = {
    //stack + tail recursive
    @annotation.tailrec
    def enstack(root: TreeNode, stack: List[TreeNode]): List[TreeNode] =
    if (root == null) stack
    else enstack(root.left, root :: stack)

    def inorderTraversal(stack: List[TreeNode], res: List[Int]): List[Int] = stack match {
      case h :: t =>
        val r = h.value :: res
        inorderTraversal(enstack(h.right, t), r)
      case _ => res.reverse
    }

    if (root == null) Nil
    else inorderTraversal(enstack(root, Nil), Nil)
  }
}
