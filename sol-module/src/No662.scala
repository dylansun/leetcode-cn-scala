/**
  * 662. 二叉树最大宽度
  */
object No662 {
  object Solution {
    type T = TreeNode
    def nextLayer(l:List[T]):List[T] = {
      l.flatMap {
        case null => List(null, null)
        case node => List(node.left, node.right)
      }.dropWhile(_==null).reverse.dropWhile(_==null)
        .reverse
    }

    def solve(l:List[T], acc:Int):Int = l match {
      case Nil => acc
      case _ => solve(nextLayer(l), acc max l.length)
    }
    def widthOfBinaryTree(root: TreeNode): Int = root match {
      case null => 0
      case _ => solve(List(root), 0)
    }
  }
}
