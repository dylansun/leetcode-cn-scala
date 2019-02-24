import javax.swing.tree.TreeNode

/**
  * Created by lilisun on 2/24/19.
  */
object No998 {
  def insertIntoMaxTree(root: TreeNode, value: Int): TreeNode = {
    if(root == null) new TreeNode(value)
    else if(value > root.value){
      val ans = new TreeNode(value)
      ans.left = root
      ans
    }else{
      root.right = insertIntoMaxTree(root.right, value)
      root
    }
  }
}
