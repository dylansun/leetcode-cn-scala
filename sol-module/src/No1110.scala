/**
  * Delete Nodes And Return Forest
  */
object No1110 {
  /**
    * Definition for a binary tree node.
    * class TreeNode(var _value: Int) {
    *   var value: Int = _value
    *   var left: TreeNode = null
    *   var right: TreeNode = null
    * }
    */
  object Solution {
    def delNodes(root: TreeNode, to_delete: Array[Int]): List[TreeNode] = {
      var to_do = List.empty[TreeNode]
      if(root == null) Nil
      else if(to_delete.contains(root.value)) delNodes(root.left, to_delete) ++ delNodes(root.right, to_delete)
      else {
        var l = List(root)
        while(l.nonEmpty){
          val h = l.head
          l = l.tail
          (h.left, h.right) match {
            case (left, right) =>
              if(left != null){
                if(to_delete.contains(left.value)){
                  h.left = null
                  to_do ::= left
                }
                else l ::= left
              }
              if(right != null) {

                if(to_delete.contains(right.value)){
                  h.right = null
                  to_do ::= right
                }
                else l ::= right
              }
          }
        }
        root::to_do.flatMap(x => delNodes(x, to_delete))
      }
    }
  }
}
