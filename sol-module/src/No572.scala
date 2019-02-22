object No572 {
  def isSubtree(s: TreeNode, t: TreeNode): Boolean = {

    def isSameTree(node1: TreeNode, node2: TreeNode): Boolean =
      if (node1 != null && node2 != null) {
        if (node1.value == node2.value) isSameTree(node1.left, node2.left) && isSameTree(node1.right, node2.right)
        else false
      } else if (node1 == null && node2 == null) true else false

    if (s == null && t == null) true
    else {
      if (isSameTree(s, t)) true
      else if (s.left != null && s.right != null) isSubtree(s.left, t) || isSubtree(s.right, t)
      else if (s.left != null) isSubtree(s.left, t)
      else if (s.right != null) isSubtree(s.right, t)
      else false
    }
  }
}
