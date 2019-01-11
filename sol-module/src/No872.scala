object No872 {
  def leafSimilar(r1: TreeNode, r2: TreeNode): Boolean = dfs(r1) == dfs(r2)

  def dfs(root: TreeNode): List[Int] = {
    if(root == null) return  List[Int]()
    if(root.left == null && root.right == null){
      return List[Int](root.value)
    }
    val left: List[Int] = if(root.left == null) List[Int]() else dfs(root.left)
    val right: List[Int] = if(root.right == null) List[Int]() else dfs(root.right)
    return left ::: right
  }
}
