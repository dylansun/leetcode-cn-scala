/**
  * Created by lilisun on 3/10/19.
  */
object No783 {

def minDiffInBST(root: TreeNode): Int = {
    var nodeValues = Set[Int]()
    def traverse(root: TreeNode): Unit = root match {
      case null => {}
      case _ => {
        nodeValues += root.value
        traverse(root.left)
        traverse(root.right)
      }
    }
    traverse(root)
    val values = nodeValues.toList.sorted
    (values zip values.tail)
    .map(x=> x._2 - x._1)
    .min
  }
}
