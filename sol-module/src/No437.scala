/**
  * Created by lilisun on 2/25/19.
  */
object No437 {
  def pathSum(root: TreeNode, sum: Int): Int = root match {
    case null => 0
    case _ => pathSum(root, sum, 0) + pathSum(root.left, sum) + pathSum(root.right, sum)
  }

  def pathSum(root: TreeNode, sum: Int, acc: Int): Int = root match {
    case null => 0
    case _ => {
      pathSum(root.left, sum, acc + root.value) + pathSum(root.right, sum, acc + root.value) + (if(root.value  + acc == sum) 1 else 0)
    }
  }
}
