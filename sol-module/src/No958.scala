/**
  * Created by lilisun on 3/14/19.
  */
object No958 {
  def isCompleteTree(root: TreeNode): Boolean = {
    var queue = List[TreeNode]()
    queue = root::queue
    while(queue.nonEmpty){
      queue.head match {
        case null => return queue.tail.forall(_ == null)
        case _ => queue = queue.tail ::: List(queue.head.left, queue.head.right)
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {

  }
}
