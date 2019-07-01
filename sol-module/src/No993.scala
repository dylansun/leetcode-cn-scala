import scala.collection.mutable
object No993 {
  object dfsSolution {
    def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
      val depth = mutable.HashMap[Int, Int]()
      val parent = mutable.HashMap[Int, TreeNode]()
      def dfs(node: TreeNode, par: TreeNode = null): Unit = {
        if (node != null) {
          depth.put(node.value, if (par == null) 0 else 1 + depth(par.value))
          parent.put(node.value, par)
          dfs(node.left, node)
          dfs(node.right, node)
        }
      }
      dfs(root)
      depth(x) == depth(y) && parent(x) != parent(y)
    }
  }
  object Solution {
    case class Node(node:TreeNode, parent:TreeNode)
    def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
      def f(l:List[Node]):Boolean = {
        if ( l.exists{case Node(n, p) => n.value == x || n.value == y}){
          l.filter{case Node(n, p) => n.value == x || n.value == y} match {
            case h::Nil => false
            case h1::h2::Nil => h1.parent != h2.parent
            case _ => false
          }
        }else
          f {
            l.flatMap {
              case Node(n, p) =>
                if (n == null) Nil
                else List(Node(n.left, n), Node(n.right, n))
                  .filter { _.node != null }
            }
          }
      }
      f(List(Node(root, null)))
    }
  }
}
