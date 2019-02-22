/**
  * Created by lilisun on 2/23/19.
  */
object No89 {
  def grayCode(n: Int): List[Int] =
    if (n <= 0) List(0)
    else {
      val prev = grayCode(n - 1)
      prev ::: prev.map(i => i | (1 << (n - 1))).reverse
    }
}
