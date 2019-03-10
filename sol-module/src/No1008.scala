/**
  * Created by lilisun on 3/10/19.
  */
object No1008 {
  def bstFromPreorder(preorder: Array[Int]): TreeNode = {
    if(preorder.isEmpty) null
    else{
      val ans = new TreeNode(preorder.head)
      ans.left = bstFromPreorder(preorder.filter(_<preorder.head))
      ans.right = bstFromPreorder(preorder.filter( _ > preorder.head))
      ans
    }
  }
}
