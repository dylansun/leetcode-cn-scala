/**
  * Created by lilisun on 3/10/19.
  */
object No897 {
  def increasingBST(root: TreeNode): TreeNode ={
   buildIncreasingBST(inOrder(root))
  }

  def buildIncreasingBST(list: List[Int]): TreeNode = list match {
    case Nil => null
    case h::t => {
      val root = new TreeNode(h)
      root.right = buildIncreasingBST(t)
      root
    }
  }
  def inOrder(root: TreeNode): List[Int] = {
    root match {
      case null => Nil
      case _ => inOrder(root.left):::(root.value::inOrder(root.right))
    }
  }
}
