/**
  * Created by lilisun on 3/7/19.
  */
object No971 {

  def flipMatchVoyage(root: TreeNode, voyage: Array[Int]): List[Int] ={
    val ans = solver(root, voyage)
    if(ans.contains(-1)) List(-1) else ans
  }
  def solver(root: TreeNode, voyage: Array[Int]): List[Int] ={
    root match {
      case null => List[Int]()
      case node: TreeNode if node.value != voyage.head => List(-1)
      case node: TreeNode if node.left == null && node.right == null => List[Int]()
      case node: TreeNode if node.left != null && node.right == null =>solver(node.left, voyage.tail)
      case node: TreeNode if node.left == null && node.right != null =>solver(node.right, voyage.tail)
      case node: TreeNode if node.left != null && node.right != null =>
      {
        val nl = countNode(root.left)
        //val nr = countNode(root.right)
        val ml = root.left.value == voyage.tail.head
        val mr = root.right.value == voyage.tail.head
        (ml, mr) match {
          case (true, false) => solver(root.left, voyage.slice(1, nl+1)) ::: solver(root.right, voyage.slice(nl+1, voyage.length))
          case (false, true) => root.value :: solver(root.right, voyage.slice(1, nl+1)) ::: solver(root.left, voyage.slice(nl+1, voyage.length))
          case _ => List(-1)
        }
      }
    }
  }

  def countNode(root: TreeNode): Int = root match {
    case null => 0
    case _ => 1 + countNode(root.left) + countNode(root.right)
  }


  // 这种编写方式很容易数组越界, 以及出现空指针的exception, 需要检查每一步
  def flipMatchVoyage2(root: TreeNode, voyage: Array[Int]): List[Int] = {
    var pos = 0
    var path = List[Int]()
    var flag = false
    def solver(root: TreeNode):Unit = {
      if(root == null){} // do nothing
      else{
        if(root.value != voyage(pos)) flag = true
        else{
          pos += 1

          if(pos < voyage.length){
            if(root.left == null && root.right != null){
              solver(root.right)
            }
            else if(root.right == null && root.left != null){
              solver(root.left)

            }
            else if(root.left == null && root.right == null){
              //do noting
            }
            else{
              if(root.left.value == voyage(pos)){
                solver(root.left)
                solver(root.right)
              }

              else if( root.right.value == voyage(pos)){
                path = path:::List(root.value)
                solver(root.right)
                solver(root.left)
              }
              else {
                flag = true
              }
            }
          }
        }
      }
    }
    solver(root)
    if(flag ) List(-1) else path
  }

  def test():Unit = {
    val root = new TreeNode((1))
    val left = new TreeNode((2))
    val ritht = new TreeNode(3)
    root.left = left
    root.right = ritht
    val path = Array(1,3,3)
    println(flipMatchVoyage(root, path))
  }

  def test1():Unit = {
    val tree = List(5,1,2,null,null,4,3)
    val path = (5,2,3,4,1)
  }

  def main(args: Array[String]): Unit = {

    test()
  }
}
