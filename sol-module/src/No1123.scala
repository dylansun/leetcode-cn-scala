/**
  *  Lowest Common Ancestor of Deepest Leaves
  */
object No1123 {
  object Solution {
    def lcaDeepestLeaves(root: TreeNode): TreeNode = {
      if(root == null) null
      else {
        val n1 = depth(root.left)
        val n2 = depth(root.right)
        if(n1 == n2) root
        else if (n1 > n2) lcaDeepestLeaves(root.left)
        else lcaDeepestLeaves(root.right)
      }
    }

    def depth(root:TreeNode):Int = {
      if (root == null) 0
      else 1 + (depth(root.left) max depth(root.right))
    }
  }
}
