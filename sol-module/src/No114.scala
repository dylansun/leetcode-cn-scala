object No114 {
  def flatten(root: TreeNode): Unit = {
    def flatten(ts: List[TreeNode]): Unit = ts match {
      case head :: tail =>
        val left = head.left
        val right = head.right
        head.left = null
        val ls =
          if (left != null && right != null)
            left :: right :: tail
          else if (right != null)
            right :: tail
          else if (left != null)
            left :: tail
          else tail
        if (ls.nonEmpty)
          head.right = ls.head
        else
          head.right = null
        flatten(ls)

      case _ => Unit
    }

    if (root != null)
      flatten(List(root))
  }
}
