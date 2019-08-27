/**
  * Created by lilisun on 8/21/19.
  */
object No1145 {
  object Solution {
    def btreeGameWinningMove(root: TreeNode, n: Int, x: Int): Boolean = {
      List(find(root,x)) flatMap { x:TreeNode =>
        List(n-count(x), count(x.left), count(x.right))
      } exists (_>n/2)
    }
    def count(root:TreeNode):Int = root match {
      case null => 0
      case _ => count(root.left) + count(root.right) + 1
    }

    def find(root:TreeNode, x:Int):TreeNode = root match {
      case null => null
      case _ if root.value == x => root
      case _ => find(root.left, x) match {
        case null => find(root.right, x)
        case node => node
      }
    }
  }
}
