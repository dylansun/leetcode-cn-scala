/**
  * Created by lilisun on 5/10/19.
  */
object No173 {
  object traverseSolution{
    case class Node (n:TreeNode, flag:Boolean)
    class BSTIterator(_root: TreeNode) {
      var l = List(Node(_root, true))
      while(l exists {case Node(_, flag) => flag}) l = l flatMap f
      val it = (l map {case Node(node, _) => node.value}) toIterator

      def f(root:Node):List[Node] = if(!root.flag) List(root) else  root.n match {
        case null => Nil
        case _ => List(Node(root.n.left, true),Node(root.n, false), Node(root.n.right, true)) filterNot {case Node(node, _) => node == null}
      }
      /** @return the next smallest number */
      def next(): Int = {
        it.next()
      }

      /** @return whether we have a next smallest number */
      def hasNext(): Boolean = {
        it.hasNext
      }

    }
  }
  object stackSolution{
    case class Node (n:TreeNode,value:Int,flag:Boolean)
    class BSTIterator(_root: TreeNode) {
      var it = Node(_root, 0, true)::Nil
      def next(): Int = it match {
        case Node(null, x, false)::t => it = t; x
        case Node(node, _, true)::t => {
          node match {
            case null => it = t; next
            case _ => it = Node(node.left, 0, true)::Node(null, node.value, false)::Node(node.right, 0, true)::t ; next
          }
        }
      }

      /** @return whether we have a next smallest number */
      def hasNext(): Boolean = it match {
        case Nil => false
        case Node(n,0,true)::t => if(n == null) {
          it = t
          hasNext
        }else true
        case Node(null, _ ,false)::t => true
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val it = List(1,2,3).toIterator
    while(it.hasNext) println(it.next())
  }
}
