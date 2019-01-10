/**
  * Created by lilisun on 1/10/19.
  */
object No563 {
  def findTilt(root: TreeNode): Int = {
    if(root == null)  return  0
    else abs(traverse(root.left) - traverse(root.right)) + findTilt(root.left) + findTilt(root.right)
  }

  def traverse(root: TreeNode): Int = {
    if(root == null) return 0
    else root.value + traverse(root.left) + traverse(root.right)
  }

  def abs(n: Int): Int = if(n < 0) -n else n
}
