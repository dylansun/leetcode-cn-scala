
object No111 {
  def minDepth(root: TreeNode):Int = {
    if(root == null) return 0
    if(root.left == null && root.right == null) return 1
    1 + minDepth2(root)
  }
  def minDepth2(root: TreeNode): Int = {
    if(root == null) return 0
    if(isleaf(root.left) || isleaf(root.right)) return 1
    if(root.left == null) return 1 + minDepth2(root.right)
    if(root.right == null) return 1 + minDepth2(root.left)
    1 + (minDepth2(root.left) min minDepth2(root.right))

  }
  def isleaf(n: TreeNode): Boolean = {
    if(n == null) return false
    if(n.left == null && n.right == null) return true
    false
  }
}
