import scala.collection.immutable.Nil

/**
  * Created by lilisun on 3/4/19.
  */
object No968 {

  def minCameraCover(root: TreeNode): Int = dp2(root).tail.min

  //{
  //  val ans = dp(root)
  //  ans._1 + 1 min ans._2 min ans._3
  //}
  def dp(root: TreeNode):(Int,Int,Int) ={
    if(root == null) return (0,0,1e9.toInt)
    val l = dp(root.left)
    val r = dp(root.right)
    val s0 = (l._2 min l._3) + (r._2 min r._3)
    val s1 =  ((l._2 min l._3) + r._3) min (l._3+(r._2 min r._3))
    val s2 = 1 + (l._1 min l._2 min l._3) + (r._1 min r._2 min r._3)
    (s0, s1, s2)
  }

  def dp2(root: TreeNode): List[Int] = {
    if(root == null) List(0,0,1e9.toInt)
    else{
      val l = dp2(root.left)
      val r = dp2(root.right)
      (l(1) +r(1)):: ((l(2)+r.tail.min) min (r(2)+l.tail.min)) :: (1 + l.min + r.min) :: Nil
    }
  }

  def main(args: Array[String]): Unit = {
    val l= List(0,2,1)
    println(l)
    println(l(2))
    println(l.tail.max)
    println(l.tail.min::0::Nil)

  }
}
