/**
  * Created by lilisun on 5/5/19.
  */
object Contest135_2 {
  class TreeNode(var _value: Int) {
      var value: Int = _value
       var left: TreeNode = null
       var right: TreeNode = null
  }
  case class Node(node:TreeNode, flag:Boolean)
  def expand(x:Node) = {
      if(x.flag) List(Node(x.node.right, true), Node(x.node, false), Node(x.node.left, true)).filter(x => x.node == null)
      else List(x)
    }
  def bstToGst(root: TreeNode): TreeNode = {
    if(root == null) return root
    var l = List(Node(root, true))
    while(l.exists(x => x.flag)){ l = l flatMap expand}

    val values = l.map(x => x.node.value)
    val cum = Array.fill(values.length)(0)
    for(i <- cum.indices){
      if(i == 0) cum(i) = values(0)
      else cum(i) = cum(i-1) + values(i)
    }
    (l zip cum).foreach( x => x._1.node.value = x._2)
    root
  }
}
