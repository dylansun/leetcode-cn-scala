/**
  * Created by lilisun on 3/10/19.
  */
object No766 {
  def isToeplitzMatrix(matrix: Array[Array[Int]]): Boolean = {
    (matrix zip matrix.tail)
          .forall( x => x._1.dropRight(1).toList == x._2.tail.toList)
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(Array(1))
    println(isToeplitzMatrix(matrix))
  }
}
