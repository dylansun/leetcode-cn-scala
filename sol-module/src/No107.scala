object No107 {

  @annotation.tailrec
  def levelOrderBottom(level:List[TreeNode], t: List[List[Int]]): List[List[Int]] ={
    if(level.isEmpty){
      return t
    }
    val nextlevel = level.foldRight(List.empty[TreeNode])((h, tl) => {
          if(h.left != null && h.right != null){
            h.left :: h.right :: tl
          }
          else if( h.left != null && h.right == null){
            h.left :: tl
          }
          else if(h.left == null && h.right != null){
            h.right :: tl
          }
          else{
            tl
          }
        }
      )
      levelOrderBottom(nextlevel, level.map(_.value)::t)
  }

  def levelOrderBottom(root: TreeNode): List[List[Int]] = {
    if(root == null) return Nil
    levelOrderBottom(List(root), List())
  }
}