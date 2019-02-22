import scala.collection.mutable
object No993 {
  def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
    val depth = mutable.HashMap[Int, Int]()
    val parent  = mutable.HashMap[Int, TreeNode]()
    def dfs(node: TreeNode, par: TreeNode = null): Unit ={
      if(node != null){
        depth.put(node.value, if(par == null) 0 else 1 + depth(par.value))
        parent.put(node.value, par)
        dfs(node.left, node)
        dfs(node.right, node)
      }
    }
    dfs(root)
    depth(x) == depth(y) && parent(x) != parent(y)
  }
}
