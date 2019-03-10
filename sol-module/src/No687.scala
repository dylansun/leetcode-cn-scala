/**
  * Created by lilisun on 3/11/19.
  */
object No687 {
  def longestUnivaluePath(root: TreeNode): Int = {
    if(root == null) return 0
    val vt = solver(root, root.value)
    val vl = longestUnivaluePath(root.left)
    val vr = longestUnivaluePath(root.right)
    (vt-1) max vl max vr
  }

  // the longest path that pass through root
  def solver(root: TreeNode, p: Int): Int = {
    if(root == null || root.value != p) return 0
    val l = countNode(root.left , p)
    val r = countNode(root.right, p)
    l + r + 1
  }

  def countNode(root: TreeNode, p: Int):Int = {
    if(root == null || root.value != p) return  0
    val l = countNode(root.left, p)
    val r = countNode(root.right, p)
    (l max r ) + 1
  }

  /*
  var maxL = 0
  def longestUnivaluePath2(root: TreeNode): Int = {

    maxL = 0
    if(root == null) return 0
    getmax(root, root.value)
    maxL
  }

  def getmax( r: TreeNode, value: Int):Int = {
    if(r == null)  return 0
    val left = getmax(r.left, r.value)
    val right = getmax(r.right, r.value)
    maxL = maxL max(left + right)
    if(r.value == value) (left max right) + 1
    else 0

  }*/

}
