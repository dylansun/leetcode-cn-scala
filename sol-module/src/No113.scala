
object No113 {
  def pathSum(root: TreeNode, target: Int): List[List[Int]] = {
    def pathSum(root: TreeNode): List[(Int, List[Int])] =
      if (root == null) List.empty
      else if (root.left == null && root.right == null) List((root.value, List(root.value)))
      else {
        val leftPathSum = pathSum(root.left)
        val rightPathSum = pathSum(root.right)
        leftPathSum.map { case (sum, path) => (sum + root.value, root.value :: path) } :::
          rightPathSum.map { case (sum, path) => (sum + root.value, root.value :: path) }
      }

    val paths = pathSum(root)
    for ((sum, path) <- paths; if sum == target)
      yield path
  }
}
