/**
  * Created by lilisun on 3/9/19.
  */
object No896 {
  def isMonotonic(A: Array[Int]): Boolean = {
    A.sorted.toList == A.toList || A.sorted.reverse.toList == A.toList
  }
}
