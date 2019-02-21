/**
  * Created by lilisun on 2/22/19.
  */
object No944 {
  def minDeletionSize(A: Array[String]): Int = {
    A.map(_.toCharArray).transpose.map(_.mkString).map(x => if(x == x.sorted) 1 else 0).sum
  }

  def main(args: Array[String]): Unit = {
    val A = Array("cba", "daf","ghi")
    println(A.mkString)
    val B = A.map(_.toCharArray).transpose.map(_.mkString)
    println(B.mkString)
  }
}
