
object No112 {
  def hasPathSum(root: TreeNode, sum: Int): Boolean = {
    def loop(root: TreeNode, target: Int): Boolean =
      if (root == null) false
      else if (root.left == null && root.right == null) target + root.value == sum
      else loop(root.left, target + root.value) || loop(root.right, target + root.value)

    loop(root, 0)
  }
}
