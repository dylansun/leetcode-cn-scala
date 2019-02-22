object No100 {
  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p != null && q != null)
      p.value == q.value && isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
    else if (p != null || q != null)
      false
    else
      true
  }
}
