object No669 {
  def trimBST(root: TreeNode, L: Int, R: Int): TreeNode = trim(root, L, R)

  def trim(node: TreeNode, L: Int, R: Int): TreeNode ={
    if(node == null)  null
    else if(node.value > R)  trim(node.left, L, R)
    else if(node.value < L)  trim(node.right, L, R)
    else{
      node.left  = trim(node.left, L, R)
      node.right = trim(node.right, L, R)
      node
    }

  }
}
