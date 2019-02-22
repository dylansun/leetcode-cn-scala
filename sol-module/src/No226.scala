
object No226 {
  def invertTree(root: TreeNode): TreeNode = {
    if (root != null) {
      val left = invertTree(root.left)
      val right = invertTree(root.right)
      root.left = right
      root.right = left
    }
    root
  }
}
