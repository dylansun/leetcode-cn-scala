/**
  * Created by lilisun on 3/3/19.
  */

import   scala.collection.mutable
object No987 {
  def verticalTraversal(root: TreeNode): List[List[Int]] = {
    val m =mutable.HashMap[Int, List[(Int,Int)]]()
    def levelOrderTraversal(root: TreeNode,key:Int, d: Int):Unit = {
      if(root != null){
        m.put(key, (root.value, d)::m.getOrElse(key, Nil))
        levelOrderTraversal(root.left, key-1, d+1)
        levelOrderTraversal(root.right, key+1, d +1)
      }
    }

    levelOrderTraversal(root, 0, 0)
    for( key <-  m.keySet.toList.sorted) yield{
      m(key).sortBy(x => (x._2, x._1)).map(_._1)
    }
  }
}
