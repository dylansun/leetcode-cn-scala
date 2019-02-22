object No145 {
  def postorderTraversal(root: TreeNode): List[Int] = {
    @annotation.tailrec
    def postorderTraversal(stack: List[TreeNode], res: List[Int]): List[Int] = stack match {
      case h :: t =>
        val r = h.value :: res
        if (h.left != null && h.right != null)
          postorderTraversal(h.right :: h.left :: t, r)
        else if (h.left != null)
          postorderTraversal(h.left :: t, r)
        else if (h.right != null)
          postorderTraversal(h.right :: t, r)
        else
          postorderTraversal(t, r)
      case _ => res
    }

    if (root == null) Nil
    else postorderTraversal(root :: Nil, Nil)
  }
}
