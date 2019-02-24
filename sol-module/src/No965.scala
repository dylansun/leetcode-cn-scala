/**
  * Created by lilisun on 2/25/19.
  */
object No965 {
  def isUnivalTree(root: TreeNode): Boolean = root match {
    case null => true
    case _ => solver(root.right, root.value) && solver(root.left, root.value)
  }

  def solver(root: TreeNode, value: Int): Boolean = {
    root match {
      case null => true
      case node: TreeNode if node.value == value => solver(node.left, value) && solver(node.right, value)
      case _ => false
    }
  }
}
