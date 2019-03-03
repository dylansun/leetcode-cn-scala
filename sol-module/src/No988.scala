/**
  * Created by lilisun on 3/3/19.
  */
object No988 {
  def smallestFromLeaf(root: TreeNode): String = {
    transAns(smallestFromLeaf(root, List[Int]()))
  }

  def transAns(ans: List[Int]):String = {
    ans.map(x => (x + 'a').toChar).mkString
  }

  def smallestFromLeaf(root: TreeNode, path: List[Int]): List[Int] = {
    if(root == null) List[Int]()
    else if(isLeaf(root)) root.value :: path
    else {
      val newpath = root.value::path

      if(root.left == null) smallestFromLeaf(root.right, newpath)

      else if(root.right == null) smallestFromLeaf(root.left, newpath)

      else if(isLeaf(root.left) && isLeaf(root.right)) (root.left.value min root.right.value)::newpath

      else if(isLeaf(root.left) && !isLeaf(root.right)) whichSmall(root.left.value::newpath, smallestFromLeaf(root.right, newpath))

      else if(!isLeaf(root.left) && isLeaf(root.right)) whichSmall(smallestFromLeaf(root.left, newpath),root.right.value::newpath)

      else whichSmall(smallestFromLeaf(root.left, newpath), smallestFromLeaf(root.right, newpath))
    }
  }
  def isLeaf(node: TreeNode): Boolean = {
    node != null && node.left ==null && node.right == null
  }

  def whichSmall(l1:List[Int],l2: List[Int]):List[Int] = {
    if(l1 == Nil || l2 == Nil) Nil
    else if(l1.head < l2.head) l1
    else if(l1.head > l2.head) l2
    else l1.head :: whichSmall(l1.tail, l2.tail)
  }

  def main(args: Array[String]): Unit = {
    val x = List(0,2,3,4)
    println(x.map(x => (x+'a').toChar))
    println(transAns(x))
  }
}
