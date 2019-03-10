/**
  * Created by lilisun on 3/10/19.
  */
object No728 {
  def selfDividingNumbers(left: Int, right: Int): List[Int] = {
    (left to right)
      .filter(isSelfDivide)
      .toList
  }
  def isSelfDivide(n: Int): Boolean = {
    n
      .toString
      .forall(x => x != '0' && n %(x - '0') == 0)
  }
}
