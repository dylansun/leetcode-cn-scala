/**
  * Created by lilisun on 3/9/19.
  */
object No905 {
  def sortArrayByParity(A: Array[Int]): Array[Int] = {
    A.filter(_%2==0) ++ A.filter(_%2 != 0)
  }
}
