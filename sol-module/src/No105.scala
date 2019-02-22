object No105 {
  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    def buildTree(prdOrderIndexed: Array[(Int, Int)], preOrderStart: Int, preOrderEnd: Int, inOrderIndexed: Array[(Int, Int)], inOrderStart: Int, inOrderEnd: Int): TreeNode =
      if (preOrderStart > preOrderEnd) null
      else {
        val value = prdOrderIndexed(preOrderStart)._1
        val root = new TreeNode(value)
        val pos = inOrderIndexed.find(_._1 == value) match {
          case Some((_, i)) => i
        }
        val leftTreeLen = pos - inOrderStart
        root.left = buildTree(prdOrderIndexed, preOrderStart + 1, preOrderStart + leftTreeLen, inOrderIndexed, inOrderStart, pos - 1)
        root.right = buildTree(prdOrderIndexed, preOrderStart + leftTreeLen + 1, preOrderEnd, inOrderIndexed, pos + 1, inOrderEnd)
        root
      }

    buildTree(preorder.zipWithIndex, 0, preorder.length - 1, inorder.zipWithIndex, 0, inorder.length - 1)
  }
}
