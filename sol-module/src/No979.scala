/**
  * Created by lilisun on 3/6/19.
  */
object No979 {
  def distributeCoins(root: TreeNode): Int = {
    if(root == null) return 0
    val l = countCoinAndNode(root.left)
    val r = countCoinAndNode(root.right)
    val cost = Math.abs(l._1 - l._2) + Math.abs(r._1 - r._2)
    cost + distributeCoins(root.left) + distributeCoins(root.right)
  }

  def countCoinAndNode(root: TreeNode): (Int, Int) = {
    if(root == null) return(0, 0)
    val l = countCoinAndNode(root.left)
    val r = countCoinAndNode(root.right)
    (1+l._1+r._1, root.value + l._2+r._2)
  }
}
