/**
  * Definition for a binary tree node.
  * class TreeNode(var _value: Int) {
  *   var value: Int = _value
  *   var left: TreeNode = null
  *   var right: TreeNode = null
  * }
  */
object No106 {
  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    def buildTree(inorderIndexed: Array[(Int, Int)], inOrderStart: Int, inOrderEnd: Int, postOrderIndexed: Array[(Int, Int)], postOrderStart: Int, postOrderEnd: Int): TreeNode =
      if (inOrderStart > inOrderEnd) null
      else {
        val value = postOrderIndexed(postOrderEnd)._1
        val root = new TreeNode(value)
        val pos = inorderIndexed.find(_._1 == value) match {
          case Some((_, i)) => i
        }
        val leftTreeLen = pos - inOrderStart
        root.left = buildTree(inorderIndexed, inOrderStart, pos - 1, postOrderIndexed, postOrderStart, postOrderStart + leftTreeLen - 1)
        root.right = buildTree(inorderIndexed, pos + 1, inOrderEnd, postOrderIndexed, postOrderStart + leftTreeLen, postOrderEnd - 1)
        root
      }

    buildTree(inorder.zipWithIndex, 0, inorder.length - 1, postorder.zipWithIndex, 0, postorder.length - 1)
  }
}
