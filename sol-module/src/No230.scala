/**
  * Created by lilisun on 6/8/19.
  */
object No230 {
  object recursionSolution {
    def kthSmallest(root: TreeNode, k: Int): Int = {
      def inorder(r:TreeNode):Array[Int] = r match {
        case null => Array.empty[Int]
        case _ => inorder(r.left) ++ Array(r.value) ++ inorder(r.right)
      }
      inorder(root)(k-1)
    }
  }

  object iterSolution {
    def kthSmallest(root: TreeNode, k: Int): Int = {
      def f(l:List[TreeNode], k:Int):Int = {
        if(k == 1) l.head.value
        else f(leftOf(l.head.right) ++ l.tail, k - 1)
      }

      def leftOf(r:TreeNode, acc:List[TreeNode] = Nil):List[TreeNode] = r match {
        case null => acc
        case _=> leftOf(r.left, r::acc)
      }

      f(leftOf(root), k)
    }
  }
}
